{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module Chesskell.Preprocessor
  ( preprocess
  ) where

import           Prelude hiding               (round)
import           Data.Char                    (ord)
import           Data.List                    (find)
import           Data.Maybe                   (isJust, isNothing, fromJust)
import           Data.Vector                  (Vector)
import qualified Data.Vector                  as Vector
import qualified Data.Matrix                  as Matrix
import qualified Data.Set                     as Set
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Control.Lens                 (makeLenses, (^.), (.~), (%~), (?~), _1, _2)
import           Chesskell.Chess              (Color (..), PieceType(..), Piece, Position, Arrangement,
                                              boardLength, initArrangement, initMainPieceI, initPawnI,
                                              initKingJ, initRookJ, castKingJ, castRookJ)
import           Chesskell.CoreCommons hiding (pieceToPosMap)
import           Chesskell.PGNParser          (RawTag (..), RawMove (..), RawGame (..),
                                              X (..), Y (..), RawExtraCoord, RawPosition)

data GameState = GameState
  { _arrangement   :: Arrangement
  , _pieceToPosMap :: Map Piece [Position]
  , _enPassantPos  :: Maybe Position
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

getBetweenPosLine :: Position -> Position -> Bool -> [Position]
getBetweenPosLine (i, j) (ii, jj) straight
  | i == ii && straight = notDiagonal (i,) j jj
  | j == jj && straight = notDiagonal (,j) i ii
  | otherwise =
    let minI = min i ii
        maxI = max i ii
        minJ = min j jj
        maxJ = max j jj
    in init $ tail [(x, y) | x <- [minI..maxI], y <- [minJ..maxJ], abs (x - i) == abs (y - j)]
  where
    notDiagonal commonApp bord1 bord2 =
      let minBord = min bord1 bord2
          maxBord = max bord1 bord2
      in init $ tail [commonApp x | x <- [minBord..maxBord]]

betweenPosLineIsClean :: Position -> Position -> Arrangement -> Bool ->  Bool
betweenPosLineIsClean fromPos toPos arrangement straight =
  let betweenPosLine = getBetweenPosLine fromPos toPos straight
  in all (\pos -> isNothing $ arrangement Matrix.! pos) betweenPosLine

getValidToPosPred :: Piece -> Position -> GameState -> Position -> Bool
getValidToPosPred (color, pieceType) toPos@(ii, jj) gameState fromPos@(i, j) =
  case pieceType of
    King   -> kingCond
    Queen  -> queenCond
    Bishop -> bishopCond
    Knight -> knightCond
    Rook   -> rookCond
    Pawn   -> pawnCond
  where
    iDist      = abs $ ii - i
    jDist      = abs $ jj - j
    pawnIDist  = (ii - i) * (if color == White then -1 else 1) 
    bpIsClean  = betweenPosLineIsClean fromPos toPos (gameState^.arrangement)
    kingCond   = iDist == 1 && jDist == 0 || iDist == 0 && jDist == 1 || iDist == 1 && jDist == 1 
    queenCond  = (iDist == 0 || jDist == 0 || iDist == jDist) && bpIsClean True
    bishopCond = iDist == jDist && bpIsClean False
    knightCond = iDist == 1 && jDist == 2 || iDist == 2 && jDist == 1
    rookCond   = (iDist == 0 || jDist == 0) && bpIsClean True
    pawnCond   =
         pawnIDist == 1 && jDist == 0 && isNothing ((gameState^.arrangement) Matrix.! toPos)
      || pawnIDist == 2 && jDist == 0 && i == initPawnI color && bpIsClean True
      || pawnIDist == 1 && jDist == 1 && isJust ((gameState^.arrangement) Matrix.! toPos)
      || pawnIDist == 1 && jDist == 1 && fmap (\(iii, jjj) -> (abs $ iii - i, abs $ jjj - j)) (gameState^.enPassantPos) == Just (0, 1)

getExtraCoordPred :: ExtraCoord -> Position -> Bool
getExtraCoordPred Nothing _            = True
getExtraCoordPred (Just (Left i)) pos  = i == pos^._1
getExtraCoordPred (Just (Right j)) pos = j == pos^._2

calcFromPos :: Piece -> ExtraCoord -> Position -> GameState -> Either String Position
calcFromPos piece@(color, pieceType) extraCoord toPos@(ii, jj) gameState =
  let candidates = (gameState^.pieceToPosMap) Map.! piece
      vpFiltered = filter validToPosPred candidates
      filtered   = filter extraCoordPred vpFiltered
  in if null filtered 
     then Left "Invalid move"
     else
      if length filtered > 1
      then Left "Ambiguous move"
      else Right $ head filtered
  where
    validToPosPred = getValidToPosPred piece toPos gameState
    extraCoordPred = getExtraCoordPred extraCoord

removeCaptured :: PieceAndPos -> GameState -> GameState
removeCaptured (captured, capturedPos) gameState =
  let updatedArgm     = arrangement %~ cleanCapturedPos $ gameState
      updatedCaptured = pieceToPosMap %~ updateCaptured $ updatedArgm
  in updatedCaptured
  where
    cleanCapturedPos = Matrix.setElem Nothing capturedPos
    updateCaptured   = Map.adjust (filter (/= capturedPos)) captured

getCaptureUpdate :: Piece -> Position -> Bool -> GameState -> Either String GameState
getCaptureUpdate (color, _) toPos wasCapture gameState =
  if wasCapture
  then
    let toPosPlace = (gameState^.arrangement) Matrix.! toPos
    in case toPosPlace of
      (Just piece) -> remove (piece, toPos)
      Nothing  -> 
        let enPassantPlace = fmap ((gameState^.arrangement) Matrix.!) (gameState^.enPassantPos)
        in case enPassantPlace of
          (Just (Just piece)) -> remove (piece, fromJust (gameState^.enPassantPos)) -- safe fromJust
          Nothing             -> Left "No captured piece for move"
  else Right gameState
  where
    remove pieceAndPos = Right $ removeCaptured pieceAndPos gameState

getEnPassantUpdate :: PieceAndPos -> Position -> GameState -> GameState
getEnPassantUpdate ((color, pieceType), fromPos@(i, _)) toPos@(ii, _) =
  if pieceType == Pawn && i == initPawnI color && abs (ii - i) == 2
  then setEnPassant
  else removeEnPassant
  where
    setEnPassant    = enPassantPos ?~ toPos
    removeEnPassant = enPassantPos .~ Nothing

getPawnTurnUpdate :: PieceAndPos -> Maybe PieceType -> GameState -> GameState
getPawnTurnUpdate (piece@(color, _), toPos) turnIntoType = case turnIntoType of
  (Just turnPieceType) -> updatePawnTurn (piece, toPos) (color, turnPieceType)
  Nothing              -> id

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

calcNextGameState :: RawMove -> Color -> GameState -> Either String (Position, Position, GameState)
calcNextGameState (BaseRawMove pieceType rawExtraCoord wasCapture rawPosition turnIntoType _ _) color gameState =
  let calcFromPosRes  = calcFromPos piece extraCoord toPos gameState
  in case calcFromPosRes of
    (Left errorMsg) -> Left errorMsg
    (Right fromPos) ->
      let captureUpdateRes = getCaptureUpdate piece toPos wasCapture gameState
      in case captureUpdateRes of 
        (Left errorMsg)           -> Left errorMsg
        (Right gsWithoutCaptured) ->
          let enPassantUpdate = getEnPassantUpdate (piece, fromPos) toPos
              moveUpdate      = makeMoveBase (piece, fromPos) toPos
              pawnTurnUpdate  = getPawnTurnUpdate (piece, toPos) turnIntoType
              nextGameState   = (pawnTurnUpdate . moveUpdate . enPassantUpdate) gsWithoutCaptured    
          in Right (fromPos, toPos, nextGameState)
  where
    piece       = (color, pieceType)
    extraCoord  = calcExtraCoord rawExtraCoord
    toPos       = calcPosition rawPosition
calcNextGameState castling color gameState =
  let castPieces = ((gameState^.arrangement) Matrix.! kingFromPos, (gameState^.arrangement) Matrix.! rookFromPos)
  in case castPieces of
    (Just king, Just rook) ->
      let nextGameState = makeMoveCastling (king, kingFromPos) (rook, rookFromPos) kingToPos rookToPos gameState
      in Right (kingFromPos, kingToPos, nextGameState)
    _                      -> Left "Castling is impossible for move"
  where
    colorCoord  = initMainPieceI color
    side        = case castling of
     ShortCastling _ _ -> Right ()
     LongCastling _ _  -> Left ()
    kingFromPos = (colorCoord, initKingJ)
    rookFromPos = (colorCoord, initRookJ side)
    kingToPos   = (colorCoord, castKingJ side)
    rookToPos   = (colorCoord, castRookJ side)

calcMovesHelper :: Int -> [RawMove] -> [Move] -> GameState -> Either String (Vector Move)
calcMovesHelper _ [] moves gameState = Right . Vector.fromList $ reverse moves
calcMovesHelper i (rm : rms) moves prevGameState =
  let color                = if even i then White else Black
      calcNextGameStateRes = calcNextGameState rm color prevGameState
  in case calcNextGameStateRes of
    (Left errorMsg)                     -> Left $ errorMsg ++ " " ++ show (i + 1)
    (Right (fromPos, toPos, gameState)) ->
      let move = Move fromPos toPos (gameState^.pieceToPosMap)
      in calcMovesHelper (i + 1) rms (move : moves) gameState

initPieceToPosMap :: Map Piece [Position]
initPieceToPosMap =
  let pieceAndPosList = foldMap toPieceAndPos [(x, y) | x <- [1..boardLength], y <- [1..boardLength]]
  in Map.fromListWith (++) pieceAndPosList
  where
    toPieceAndPos pos =
      case initArrangement Matrix.! pos of
      Nothing      -> []
      (Just piece) -> [(piece, [pos])]

calcMoves :: [RawMove] -> Either String (Vector Move)
calcMoves rawMoves = calcMovesHelper 0 rawMoves [] initialGameState
  where
    initialGameState = GameState
      { _arrangement   = initArrangement
      , _pieceToPosMap = initPieceToPosMap
      , _enPassantPos  = Nothing
      }

preprocessHelper :: Int -> [RawGame] -> [Game] -> Either String (Vector Game)
preprocessHelper _ [] games         = Right . Vector.fromList $ reverse games
preprocessHelper i (rg : rgs) games =
  let calcMovesRes = calcMoves $ rawMoves rg
  in case calcMovesRes of
    (Left errorMsg)  -> Left $ errorMsg ++ " at game " ++ show (i + 1)
    (Right moves)    ->
      let game = Game
            { tags   = calcTags $ rawTags rg
            , moves  = moves
            , winner = rawWinner rg
            }
      in preprocessHelper (i + 1) rgs (game : games)

preprocess :: [RawGame] -> Either String (Vector Game)
preprocess rawGames = preprocessHelper 0 rawGames []