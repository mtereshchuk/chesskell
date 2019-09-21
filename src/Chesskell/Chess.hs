module Chesskell.Chess
  ( Color (..)
  , PieceType (..)
  , Piece
  , Position
  , Place
  , Arrangement
  , chessBoardLength
  , oppositeColor
  , getAllPieces
  , initialArrangement
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

chessBoardLength :: Int
chessBoardLength = 8

oppositeColor :: Color -> Color
oppositeColor White = Black
oppositeColor Black = White

getAllPieces :: [Piece]
getAllPieces = [(c, pt) | c <- [White, Black], pt <- [King, Queen, Bishop, Knight, Rook, Pawn]]

initialArrangement :: Arrangement
initialArrangement = Matrix.fromLists $
     [toBlack <$> mainPieceRow]
  ++ [toBlack <$> pawnRow]
  ++ replicate (chessBoardLength - 4) emptyRaw
  ++ [toWhite <$> pawnRow]
  ++ [toWhite <$> mainPieceRow]
    where
      mainPieceRow = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
      pawnRow      = replicate chessBoardLength Pawn
      emptyRaw     = replicate chessBoardLength Nothing
      toBlack      = Just . (,) Black
      toWhite      = Just . (,) White