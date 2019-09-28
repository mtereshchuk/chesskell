module Chesskell.Chess
  ( Color (..)
  , PieceType (..)
  , Piece
  , Position
  , Place
  , Arrangement
  , boardLength
  , oppositeColor
  , allPieces
  , initArrangement
  , initMainPieceI
  , initPawnI
  , initKingJ
  , initRookJ
  , castKingJ
  , castRookJ
  , arrangementToString
  ) where

import           Data.Char   (toLower)
import           Data.List   (intercalate)
import           Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix

data Color = White | Black 
  deriving (Eq, Ord, Show)

data PieceType = King | Queen | Bishop | Knight | Rook | Pawn
  deriving (Eq, Ord, Show)

type Piece = (Color, PieceType)
type Position = (Int, Int)
type Place = Maybe Piece
type Arrangement = Matrix Place

boardLength :: Int
boardLength = 8

oppositeColor :: Color -> Color
oppositeColor White = Black
oppositeColor Black = White

allPieces :: [Piece]
allPieces = [(c, pt) | c <- [White, Black], pt <- [King, Queen, Bishop, Knight, Rook, Pawn]]

initArrangement :: Arrangement
initArrangement = Matrix.fromLists $
     [toBlack <$> mainPieceRow]
  ++ [toBlack <$> pawnRow]
  ++ replicate (boardLength - 4) emptyRaw
  ++ [toWhite <$> pawnRow]
  ++ [toWhite <$> mainPieceRow]
    where
      mainPieceRow = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
      pawnRow      = replicate boardLength Pawn
      emptyRaw     = replicate boardLength Nothing
      toBlack      = Just . (,) Black
      toWhite      = Just . (,) White

initMainPieceI :: Color -> Int
initMainPieceI White = 8
initMainPieceI Black = 1

initPawnI :: Color -> Int
initPawnI White = 7
initPawnI Black = 2

initKingJ :: Int
initKingJ = 5

initRookJ :: Either () () -> Int
initRookJ (Left _)  = 1
initRookJ (Right _) = 8

castKingJ :: Either () () -> Int
castKingJ (Left _)  = 3
castKingJ (Right _) = 7

castRookJ :: Either () () -> Int
castRookJ (Left _)  = 4
castRookJ (Right _) = 6

placeToChar :: Place -> Char
placeToChar Nothing = '.'
placeToChar (Just (color, figureType)) =
  colorTransform color $ toSymbol figureType
  where
    toSymbol ft =
      if ft == Knight
      then 'N'
      else head $ show ft
    colorTransform c =
      if c == Black
      then toLower
      else id

arrangementToString :: Arrangement -> String
arrangementToString arrangement = intercalate "\n" $ map placeToChar <$> Matrix.toLists arrangement