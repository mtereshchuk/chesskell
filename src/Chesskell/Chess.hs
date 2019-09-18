module Chesskell.Chess
    ( Color (..)
    , FigureType (..)
    , Figure
    , Position
    , Place
    , Arrangement
    , chessBoardLength
    , oppositeColor
    , initialArrangement
    , arrangementToString
    , getAllFigures
    ) where

import Data.Char (toLower)
import Data.List (intercalate)
import qualified Data.Matrix as Matrix
import Data.Matrix (Matrix)

data Color = White | Black 
    deriving (Eq, Ord, Show)

data FigureType = King | Queen | Bishop | Knight | Rook | Pawn 
    deriving (Eq, Ord, Show)

type Figure = (Color, FigureType)
type Position = (Int, Int)
type Place = Maybe Figure
type Arrangement = Matrix Place

chessBoardLength :: Int
chessBoardLength = 8

oppositeColor :: Color -> Color
oppositeColor White = Black
oppositeColor Black = White

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

placeToChar :: Maybe Figure -> Char
placeToChar Nothing = '.'
placeToChar (Just (color, figureType)) = 
    colorTransform color $ toSymbol figureType
    where toSymbol ft = if (ft == Knight) 
            then 'N' 
            else head $ show ft
          colorTransform c = if (c == Black) 
            then toLower
            else id 

arrangementToString :: Arrangement -> String
arrangementToString arrangement = intercalate "\n" $ map placeToChar <$> Matrix.toLists arrangement

getAllFigures :: [Figure]
getAllFigures = [(x, y) | x <- [White, Black], y <- [King, Queen, Bishop, Knight, Rook, Pawn]]