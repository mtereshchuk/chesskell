module Chesskell.ChessCommons 
    ( Color (..)
    , FigureType (..)
    , Tags (..)
    , Move (..)
    , Game (..)
    , Figure
    , Place
    , Position
    , Arrangement
    , chessBoardLength
    , initialArrangement
    , oppositeColor
    , arrangementToString
    ) where

import Data.Char (toLower)
import Data.List (intercalate)
import Data.Matrix

data Color = White | Black 
    deriving (Eq, Show, Read)

data FigureType = King | Queen | Rook | Knight | Bishop | Pawn 
    deriving (Eq, Show, Read)

data Tags = Tags
    { event  :: Maybe String
    , site   :: Maybe String
    , date   :: Maybe String
    , round  :: Maybe String
    , white  :: Maybe String
    , black  :: Maybe String
    , result :: Maybe String
    } deriving (Eq, Show, Read)

data Move 
    = BaseMove
    { figure         :: Figure
    , fromPos        :: Position
    , toPos          :: Position
    }
    | EnPassant
    { color           :: Color
    , fromPos         :: Position
    , toPos           :: Position
    , capturedPawnPos :: Position
    }
    | Castling
    { color       :: Color
    , kingFromPos :: Position
    , kingToPos   :: Position
    , rookFromPos :: Position
    , rookToPos   :: Position
    }

data Game = Game
    { tags   :: Tags
    , moves  :: [Move]
    , winner :: Maybe Color
    }

type Figure = (Color, FigureType)
type Place = Maybe Figure
type Position = (Int, Int)
type Arrangement = Matrix Place

chessBoardLength :: Int
chessBoardLength = 8

initialArrangement :: Arrangement
initialArrangement = fromLists $
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

oppositeColor :: Color -> Color
oppositeColor White = Black
oppositeColor Black = White

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
arrangementToString arrangement = intercalate "\n" $ map placeToChar <$> toLists arrangement