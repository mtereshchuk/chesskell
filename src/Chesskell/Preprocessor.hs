{-# LANGUAGE TemplateHaskell #-}

module Chesskell.Preprocessor where

import Data.Maybe (fromJust)
import Prelude hiding (round)
import Data.List      (find, iterate)
import Data.Char
import qualified Data.Matrix as Matrix hiding (fromList) 
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map hiding (take, map, filter)
import Control.Lens
import Chesskell.ChessCommons
import Chesskell.PGNParser
--import Chesskell.Util

data GameState = GameState
    { _arrangement :: Arrangement
    , _figToPosMap :: Map Figure [Position]
    , _enPassantOp :: Maybe Position
    }

type ExtraCoord = Maybe (Either Int Int)
type Position = (Int, Int) 
type FigAndPos = (Figure, Position)

makeLenses ''GameState

calcTags :: [RawTag] -> Tags
calcTags rawTags = 
    let findTagValue name = tagValue <$> find (\t -> tagName t == name) rawTags
    in Tags 
        { event  = findTagValue "Event"
        , site   = findTagValue "Site"
        , date   = findTagValue "Date"
        , round  = findTagValue "Round"
        , white  = findTagValue "White"
        , black  = findTagValue "Black"
        , result = findTagValue "Result"
        }

calcI :: X -> Int
calcI (X letter) = ord letter - ord 'a'

caclJ :: Y -> Int
caclJ (Y digit) = 8 - digit

calcExtraCoord :: RawExtraCoord -> ExtraCoord
calcExtraCoord Nothing          = Nothing
calcExtraCoord (Just (Left x))  = Just . Left  $ calcI x
calcExtraCoord (Just (Right y)) = Just . Right $ caclJ y

calcPosition :: RawPosition -> Position
calcPosition (x, y) = (calcI x, caclJ y)

getExtraCoordPred :: ExtraCoord -> Position -> Bool
getExtraCoordPred Nothing _            = True
getExtraCoordPred (Just (Left i)) pos  = i == pos^._1
getExtraCoordPred (Just (Right j)) pos = j == pos^._2

validToPosPawnPred :: Position -> Position -> Color -> GameState -> Bool
validToPosPawnPred (i, j) toPos@(ii, jj) color gameState
    | abs (ii - i) == 1 && j == jj && (gameState^.arrangement) Matrix.! toPos == Nothing                    = True
    | abs (ii - i) == 2 && i == colorCoord && j == jj && (gameState^.arrangement) Matrix.! toPos == Nothing = True
    | abs (ii - i) == 1 && abs (jj - j) == 1 && (gameState^.arrangement) Matrix.! toPos /= Nothing          = True
    | abs (ii - i) == 1 && abs (jj - j) == 1 && gameState^.enPassantOp == (Just toPos)                      = True
    | otherwise                                                                                             = False
    where colorCoord = if (color == White) then 6 else 1

getBetweenPosLine :: Position -> Position -> [Position]
getBetweenPosLine (i, j) (ii, jj)
    | i == ii   = notDiagonal ((,) i) j jj
    | j == jj   = notDiagonal (\x -> (x, j)) i ii
    | otherwise = 
        let minI = min i ii
            maxI = max i ii
            minJ = min j jj
            maxJ = max j jj
        in init $ tail [(x, y) | x <- [minI..maxI], y <- [minJ..maxJ], x - y == i - j]
    where notDiagonal commonApp bord1 bord2 = 
            let minBord = min bord1 bord2
                maxBord = max bord1 bord2
            in init $ tail [commonApp x | x <- [minBord..maxBord]]

betweenPosLineIsClean :: Position -> Position -> Arrangement -> Bool
betweenPosLineIsClean fromPos toPos arrangement =
    let betweenPosLine = getBetweenPosLine fromPos toPos
    in all (\pos -> arrangement Matrix.! pos == Nothing) betweenPosLine

calcFromPos :: Figure -> ExtraCoord -> Position -> GameState -> Position
calcFromPos figure@(color, figureType) extraCoord toPos@(ii, jj) gameState =
    let candidates = (gameState^.figToPosMap) Map.! figure
        ecFiltered = filter extraCoordPred candidates
        filtered   = filter validToPosPred ecFiltered
    in head filtered
    where extraCoordPred = getExtraCoordPred extraCoord
          validToPosPred = case figureType of
            King   -> \_ -> True
            Knight -> \(i, j) ->
                let dist = (abs $ ii - i, abs $ jj - j)
                in dist == (1, 2) || dist == (2, 1)
            Pawn   -> undefined
            _      -> \fromPos -> 
                betweenPosLineIsClean fromPos toPos (gameState^.arrangement)

makeMoveBase :: FigAndPos -> Position -> GameState -> GameState
makeMoveBase (figure, fromPos) toPos gameState = 
    let updatedArgm  = arrangement %~ (cleanFromPos . fillToPos) $ gameState
        updatedState = figToPosMap %~ updateFigure $ updatedArgm
    in updatedState
    where cleanFromPos = Matrix.setElem Nothing fromPos
          fillToPos    = Matrix.setElem (Just figure) toPos
          updateFigure = Map.adjust ((toPos :) . filter (/= fromPos)) figure

makeMoveCapture :: FigAndPos -> FigAndPos -> Position -> GameState -> GameState
makeMoveCapture figAndPos (captured, capturedPos) toPos gameState = 
    let updatedArgm     = arrangement %~ cleanCapturedPos $ gameState
        updatedCaptured = figToPosMap %~ updateCaptured $ updatedArgm
    in makeMoveBase figAndPos toPos updatedCaptured
    where cleanCapturedPos = Matrix.setElem Nothing capturedPos
          updateCaptured   = Map.adjust (filter (/= capturedPos)) captured

makeMoveEnPassant :: FigAndPos -> Position -> Position -> GameState -> GameState
makeMoveEnPassant figAndPos enPassantPos toPos gameState = 
    let updatedEnPasOp  = enPassantOp .~ (Just enPassantPos) $ gameState
    in makeMoveBase figAndPos toPos updatedEnPasOp

makeMoveCastling :: FigAndPos -> FigAndPos -> Position -> Position -> GameState -> GameState
makeMoveCastling (king, kingFromPos) (rook, rookFromPos) kingToPos rookToPos gameState = 
    let updatedArgm  = arrangement %~ (cleanKingFromPos . fillKingToPos . cleanRookFromPos . fillRookToPos) $ gameState
        updatedState = figToPosMap %~ (updateKing . updateRook) $ updatedArgm
    in updatedState
    where cleanKingFromPos = Matrix.setElem Nothing kingFromPos
          fillKingToPos    = Matrix.setElem (Just king) kingToPos 
          cleanRookFromPos = Matrix.setElem Nothing rookFromPos
          fillRookToPos    = Matrix.setElem (Just rook) rookToPos
          updateKing       = Map.adjust ((kingToPos :) . filter (/= kingFromPos)) king
          updateRook       = Map.adjust ((rookToPos :) . filter (/= rookFromPos)) rook

calcNextGameState :: RawMove -> Color -> GameState -> GameState
calcNextGameState (BaseRawMove figureType rawExtraCoord wasCapture rawPosition turnIntoType _ _) color gameState
    | wasCapture = 
        let toPosFigure     = (gameState^.arrangement) Matrix.! capturedPos
            enPassantChange = \coord -> if (color == White) then coord + 1 else coord - 1
            capturedPos     = case toPosFigure of 
                    (Just _) -> toPos
                    _        -> _1 %~ enPassantChange $ toPos
            captured        = fromJust ((gameState^.arrangement) Matrix.! capturedPos)
        in makeMoveCapture (figure, fromPos) (captured, capturedPos) toPos gameState
    | figureType == Pawn && i == (if (color == White) then 6 else 1) && abs (ii - i) == 2 =
        makeMoveEnPassant (figure, fromPos) (if (color == White) then (ii - 1, jj) else (ii + 1, jj)) toPos gameState
    | otherwise  = makeMoveBase (figure, fromPos) toPos gameState
    where figure          = (color, figureType)
          extraCoord      = calcExtraCoord rawExtraCoord
          toPos@(ii, jj)  = calcPosition rawPosition
          fromPos@(i, j)  = calcFromPos figure extraCoord toPos gameState
calcNextGameState castling color gameState =
    makeMoveCastling (king, kingFromPos) (rook, rookFromPos) kingToPos rookToPos gameState
    where colorCoord  = if (color == White) then 7 else 0 
          king        = (color, King)
          kingFromPos = (colorCoord, 4)
          rook        = (color, Rook)
          rookFromPos = case castling of 
            ShortCastling _ _ -> (colorCoord, 7)
            _                 -> (colorCoord, 0)
          kingToPos   = case castling of 
            ShortCastling _ _ -> (colorCoord, 6)
            _                 -> (colorCoord, 2)
          rookToPos   = case castling of 
            ShortCastling _ _ -> (colorCoord, 5)
            _                 -> (colorCoord, 3)

calcArrangementsHelper :: [RawMove] -> [Arrangement] -> Color -> GameState -> [Arrangement]
calcArrangementsHelper [] argms _ _                     = argms
calcArrangementsHelper (rm : rms) argms color gameState = 
    let newArgms      = argms ++ [gameState^.arrangement]
        nextColor     = oppositeColor color
        nextGameState = calcNextGameState rm color gameState
    in calcArrangementsHelper rms newArgms nextColor nextGameState

initialArrangement :: Arrangement
initialArrangement = Matrix.fromLists $
       [toBlack <$> mainFigureRow]
    ++ [toBlack <$> pawnRow]
    ++ replicate (chessBoardLength - 4) emptyRaw
    ++ [toWhite <$> pawnRow]
    ++ [toWhite <$> mainFigureRow]
    where
        mainFigureRow = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
        pawnRow       = replicate chessBoardLength Pawn
        emptyRaw      = replicate chessBoardLength Nothing
        toBlack       = Just . ((,) Black)
        toWhite       = Just . ((,) White)

initialFigToPosMap :: Map Figure [Position]
initialFigToPosMap = Map.fromList
    [ ((White, King)  , [(7, 4)])
    , ((White, Queen) , [(7, 3)])
    , ((White, Rook)  , [(7, 2), (7, 5)])
    , ((White, Knight), [(7, 1), (7, 6)])
    , ((White, Bishop), [(7, 0), (7, 7)])
    , ((White, Pawn)  , take 8 $ iterate (_2 %~ (+1)) (6, 0))
    , ((Black, King)  , [(0, 4)])
    , ((Black, Queen) , [(0, 3)])
    , ((Black, Rook)  , [(0, 2), (0, 5)])
    , ((Black, Knight), [(0, 1), (0, 6)])
    , ((Black, Bishop), [(0, 0), (0, 7)])
    , ((Black, Pawn)  , take 8 $ iterate (_2 %~ (+1)) (1, 0))
    ]

calcArrangements :: [RawMove] -> [Arrangement]
calcArrangements rawMoves = calcArrangementsHelper rawMoves [] White initialGameState
    where initialGameState = GameState 
            { _arrangement = initialArrangement
            , _figToPosMap = initialFigToPosMap
            , _enPassantOp = Nothing
            }

calcGame :: RawGame -> Game
calcGame rawGame = Game 
    { tags         = calcTags $ rawTags rawGame
    , arrangements = calcArrangements $ rawMoves rawGame
    , winner       = rawWinner rawGame
    }