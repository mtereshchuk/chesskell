{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Chesskell.Preprocessor
  ( preprocessRawGames
  ) where

import           Prelude hiding           (round)
import           Data.Char                (ord)
import           Data.List                (find)
import           Data.Maybe               (fromJust, isNothing, isJust)
import           Data.Vector              (Vector)
import qualified Data.Vector              as Vector
import qualified Data.Matrix              as Matrix
import qualified Data.Set                 as Set
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Control.Lens             (makeLenses, (^.), (.~), (%~), (?~), _1, _2)
import           Chesskell.Chess          (Color (..), PieceType(..), Piece, Position, Arrangement,
                                          boardLength, oppositeColor, initialArrangement)
import           Chesskell.Commons hiding (pieceToPosMap)
import           Chesskell.PGNParser      (RawTag (..), RawMove (..), RawGame (..),
                                          X (..), Y (..), RawExtraCoord, RawPosition)

data GameState = GameState
  { _arrangement   :: Arrangement
  , _pieceToPosMap :: Map Piece [Position]
  , _enPassantOp   :: Maybe Position
  }

type ExtraCoord = Maybe (Either Int Int)
type PieceAndPos = (Piece, Position)

makeLenses ''GameState

calcTags :: [RawTag] -> [Tag]
calcTags rawTags = 
  let isSupportedTag (RawTag name _) = Set.member name supportedTagNames
      toTag (RawTag name value)      = Tag name value 
  in map toTag $ filter isSupportedTag rawTags

calcI :: Y -> Int
calcI (Y digit) = 8 - digit + 1

calcJ :: X -> Int
calcJ (X letter) = (ord letter - ord 'a') + 1

calcExtraCoord :: RawExtraCoord -> ExtraCoord
calcExtraCoord Nothing          = Nothing
calcExtraCoord (Just (Left x))  = Just . Right $ calcJ x
calcExtraCoord (Just (Right y)) = Just . Left  $ calcI y

calcPosition :: RawPosition -> Position
calcPosition (x, y) = (calcI y, calcJ x)

getExtraCoordPred :: ExtraCoord -> Position -> Bool
getExtraCoordPred Nothing _            = True
getExtraCoordPred (Just (Left i)) pos  = i == pos^._1
getExtraCoordPred (Just (Right j)) pos = j == pos^._2

validToPosPawnPred :: Position -> Position -> Color -> GameState -> Bool
validToPosPawnPred (i, j) toPos@(ii, jj) color gameState
  | abs (ii - i) == 1 && j == jj && isNothing ((gameState^.arrangement) Matrix.! toPos)                    = True
  | abs (ii - i) == 2 && i == colorCoord && j == jj && isNothing ((gameState^.arrangement) Matrix.! toPos) = True
  | abs (ii - i) == 1 && abs (jj - j) == 1 && isJust ((gameState^.arrangement) Matrix.! toPos)             = True
  | abs (ii - i) == 1 && abs (jj - j) == 1 && gameState^.enPassantOp == Just toPos                         = True
  | otherwise                                                                                              = False
  where colorCoord = if color == White then 7 else 2

getBetweenPosLine :: Position -> Position -> Bool -> [Position]
getBetweenPosLine (i, j) (ii, jj) straight
  | i == ii && straight = notDiagonal (i,) j jj
  | j == jj && straight = notDiagonal (,j) i ii
  | otherwise =
    let minI = min i ii
        maxI = max i ii
        minJ = min j jj
        maxJ = max j jj
        genList = [(x, y) | x <- [minI..maxI], y <- [minJ..maxJ], abs (x - i) == abs (y - j)]
        tailed = tail genList
        inited = init tailed
    in inited
  where
    notDiagonal commonApp bord1 bord2 =
      let minBord = min bord1 bord2
          maxBord = max bord1 bord2
      in init $ tail [commonApp x | x <- [minBord..maxBord]]

betweenPosLineIsClean :: Position -> Position -> Bool -> Arrangement -> Bool
betweenPosLineIsClean fromPos toPos straight arrangement =
  let betweenPosLine = getBetweenPosLine fromPos toPos straight
  in all (\pos -> isNothing $ arrangement Matrix.! pos) betweenPosLine

calcFromPos :: Piece -> ExtraCoord -> Position -> GameState -> Position
calcFromPos piece@(color, pieceType) extraCoord toPos@(ii, jj) gameState =
  let candidates = (gameState^.pieceToPosMap) Map.! piece
      ecFiltered = filter extraCoordPred candidates
      filtered   = filter validToPosPred ecFiltered
  in head filtered
  where 
    extraCoordPred = getExtraCoordPred extraCoord
    validToPosPred = case pieceType of
      King   -> const True
      Queen  -> \fromPos@(i, j) -> (i == ii || j == jj || abs (i - ii) == abs (jj - j))
        && betweenPosLineIsClean fromPos toPos True (gameState^.arrangement)
      Bishop -> \fromPos@(i, j) -> abs (i - ii) == abs (jj - j)
        && betweenPosLineIsClean fromPos toPos False (gameState^.arrangement)
      Knight -> \(i, j) ->
        let dist = (abs $ ii - i, abs $ jj - j)
        in dist == (1, 2) || dist == (2, 1)
      Rook   -> \fromPos@(i, j) -> (i == ii || j == jj)
        && betweenPosLineIsClean fromPos toPos True (gameState^.arrangement)
      _      -> \fromPos -> validToPosPawnPred fromPos toPos color gameState

removeCaptured :: PieceAndPos -> GameState -> GameState
removeCaptured (captured, capturedPos) gameState = 
  let updatedArgm     = arrangement %~ cleanCapturedPos $ gameState
      updatedCaptured = pieceToPosMap %~ updateCaptured $ updatedArgm
  in updatedCaptured
  where 
    cleanCapturedPos = Matrix.setElem Nothing capturedPos
    updateCaptured   = Map.adjust (filter (/= capturedPos)) captured

setEnPassant :: Position -> GameState -> GameState
setEnPassant enPassantPos = enPassantOp ?~ enPassantPos

removeEnPassant :: GameState -> GameState
removeEnPassant = enPassantOp .~ Nothing

getCaptureUpdate :: Piece -> Position -> Bool -> GameState -> GameState
getCaptureUpdate (color, _) toPos wasCapture gameState = if wasCapture
  then
    let toPosPiece            = (gameState^.arrangement) Matrix.! toPos
        enPassantChange coord = if color == White then coord + 1 else coord - 1
        capturedPos           = case toPosPiece of
          (Just _) -> toPos
          _        -> _1 %~ enPassantChange $ toPos
        captured              = fromJust ((gameState^.arrangement) Matrix.! capturedPos)
    in removeCaptured (captured, capturedPos) gameState
  else gameState

getEnPassantUpdate :: PieceAndPos -> Position -> GameState -> GameState
getEnPassantUpdate ((color, pieceType), fromPos@(i, j)) toPos@(ii, jj) =
  if pieceType == Pawn && i == (if color == White then 7 else 2) && abs (ii - i) == 2
  then setEnPassant (if color == White then (ii + 1, jj) else (ii - 1, jj))
  else removeEnPassant

getPawnTurnUpdate :: PieceAndPos -> Maybe PieceType -> GameState -> GameState
getPawnTurnUpdate (piece@(color, _), toPos) turnIntoType = case turnIntoType of
  (Just turnPieceType) -> updatePawnTurn (piece, toPos) (color, turnPieceType)
  Nothing               -> id

makeMoveBase :: PieceAndPos -> Position -> GameState -> GameState
makeMoveBase (piece, fromPos) toPos gameState = 
  let updatedArgm  = arrangement %~ (fillToPos . cleanFromPos) $ gameState
      updatedState = pieceToPosMap %~ updatePiece $ updatedArgm
  in updatedState
  where 
    cleanFromPos = Matrix.setElem Nothing fromPos
    fillToPos    = Matrix.setElem (Just piece) toPos
    updatePiece  = Map.adjust ((toPos :) . filter (/= fromPos)) piece

updatePawnTurn :: PieceAndPos -> Piece -> GameState -> GameState
updatePawnTurn (pawn, pawnPos) piece gameState =
  let updatedArgm   = arrangement %~ replacePiece $ gameState
      updatedTurned = pieceToPosMap %~ (addPiecePos . removePawnPos) $ updatedArgm
  in updatedTurned
  where
    replacePiece  = Matrix.setElem (Just piece) pawnPos
    removePawnPos = Map.adjust (filter (/= pawnPos)) pawn
    addPiecePos   = Map.adjust (pawnPos :) piece

makeMoveCastling :: PieceAndPos -> PieceAndPos -> Position -> Position -> GameState -> GameState
makeMoveCastling (king, kingFromPos) (rook, rookFromPos) kingToPos rookToPos gameState = 
  let updatedArgm  = arrangement %~ (fillRookToPos . cleanRookFromPos . fillKingToPos . cleanKingFromPos) $ gameState
      updatedState = pieceToPosMap %~ (updateRook . updateKing) $ updatedArgm
  in updatedState
  where
    cleanKingFromPos = Matrix.setElem Nothing kingFromPos
    fillKingToPos    = Matrix.setElem (Just king) kingToPos
    cleanRookFromPos = Matrix.setElem Nothing rookFromPos
    fillRookToPos    = Matrix.setElem (Just rook) rookToPos
    updateKing       = Map.adjust ((kingToPos :) . filter (/= kingFromPos)) king
    updateRook       = Map.adjust ((rookToPos :) . filter (/= rookFromPos)) rook

calcNextGameState :: RawMove -> Color -> GameState -> (Position, Position, GameState)
calcNextGameState (BaseRawMove pieceType rawExtraCoord wasCapture rawPosition turnIntoType _ _) color gameState =
  let captureUpdate   = getCaptureUpdate piece toPos wasCapture
      enPassantUpdate = getEnPassantUpdate (piece, fromPos) toPos
      moveUpdate      = makeMoveBase (piece, fromPos) toPos
      pawnTurnUpdate  = getPawnTurnUpdate (piece, toPos) turnIntoType
      nextGameState   = (pawnTurnUpdate . moveUpdate . enPassantUpdate . captureUpdate) gameState
  in (fromPos, toPos, nextGameState)
  where
    piece          = (color, pieceType)
    extraCoord     = calcExtraCoord rawExtraCoord
    toPos@(ii, jj) = calcPosition rawPosition
    fromPos@(i, j) = calcFromPos piece extraCoord toPos gameState
calcNextGameState castling color gameState =
  let nextGameState = makeMoveCastling (king, kingFromPos) (rook, rookFromPos) kingToPos rookToPos gameState
  in (kingFromPos, kingToPos, nextGameState)
  where
    colorCoord  = if color == White then 8 else 1
    king        = (color, King)
    kingFromPos = (colorCoord, 5)
    rook        = (color, Rook)
    rookFromPos = case castling of
      ShortCastling _ _ -> (colorCoord, 8)
      _                 -> (colorCoord, 1)
    kingToPos   = case castling of
      ShortCastling _ _ -> (colorCoord, 7)
      _                 -> (colorCoord, 3)
    rookToPos   = case castling of
      ShortCastling _ _ -> (colorCoord, 6)
      _                 -> (colorCoord, 4)

calcMovesHelper :: [RawMove] -> [Move] -> Color -> GameState -> Vector Move
calcMovesHelper [] moves _ gameState = Vector.fromList $ reverse moves
calcMovesHelper (rm : rms) moves prevColor prevGameState = 
  let color                       = oppositeColor prevColor
      (fromPos, toPos, gameState) = calcNextGameState rm color prevGameState
      newMove                     = Move fromPos toPos (gameState^.pieceToPosMap) 
  in calcMovesHelper rms (newMove : moves) color gameState

initialPieceToPosMap :: Map Piece [Position]
initialPieceToPosMap =
  let pieceAndPosList = foldMap toPieceAndPos [(x, y) | x <- [1..boardLength], y <- [1..boardLength]]
  in Map.fromListWith (++) pieceAndPosList
  where
    toPieceAndPos pos =
      case initialArrangement Matrix.! pos of
      Nothing      -> []
      (Just piece) -> [(piece, [pos])]

calcMoves :: [RawMove] -> Vector Move
calcMoves rawMoves = calcMovesHelper rawMoves [] Black initialGameState
  where
    initialGameState = GameState
      { _arrangement   = initialArrangement
      , _pieceToPosMap = initialPieceToPosMap
      , _enPassantOp   = Nothing
      }

preprocessRawGames :: [RawGame] -> Vector Game
preprocessRawGames = Vector.fromList . map preprocessRawGame
  where
    preprocessRawGame rawGame = Game
      { tags   = calcTags  $ rawTags rawGame
      , moves  = calcMoves $ rawMoves rawGame
      , winner = rawWinner rawGame
      }