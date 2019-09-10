module Chesskell.ChessCommons
    ( Color (..)
    , FigureType (..)
    , Tags (..)
    , Game (..)
    , Figure
    , Place
    , Arrangement
    , chessBoardLength
    , oppositeColor
    , arrangementToString
    ) where

import Data.Char (toLower)
import Data.List (intercalate)
import Data.Matrix

data Color = White | Black 
    deriving (Eq, Ord, Show, Read)

data FigureType = King | Queen | Bishop | Knight | Rook | Pawn 
    deriving (Eq, Ord, Show, Read)

data Tags = Tags
    { event  :: Maybe String
    , site   :: Maybe String
    , date   :: Maybe String
    , round  :: Maybe String
    , white  :: Maybe String
    , black  :: Maybe String
    , result :: Maybe String
    } deriving (Eq, Show, Read)

data Game = Game
    { tags         :: Tags
    , arrangements :: [Arrangement]
    , winner       :: Maybe Color
    }

type Figure = (Color, FigureType)
type Place = Maybe Figure
type Arrangement = Matrix Place

chessBoardLength :: Int
chessBoardLength = 8

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