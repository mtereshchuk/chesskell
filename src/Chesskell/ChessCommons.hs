module Chesskell.ChessCommons 
    ( Color (..)
    , FigureType (..)
    , Figure
    , Arrangement
    , chessBoardLength
    , initialArrangement
    , arrangementToString
    ) where

import Data.Char (toLower)
import Data.List (intercalate)

data Color = White | Black 
    deriving (Eq, Show, Read)

data FigureType = King | Queen | Rook | Knight | Bishop | Pawn 
    deriving (Eq, Show, Read)

type Figure = (Color, FigureType)
type Arrangement = [[Maybe Figure]]

chessBoardLength :: Int
chessBoardLength = 8

initialArrangement :: Arrangement
initialArrangement = 
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
    where 
        toSymbol ft = if (ft == Knight) 
            then 'N' 
            else head $ show ft
        colorTransform c = if (c == Black)
            then toLower
            else id 

arrangementToString :: Arrangement -> String
arrangementToString arrangement = intercalate "\n" $ map placeToChar <$> arrangement