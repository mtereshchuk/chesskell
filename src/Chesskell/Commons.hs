module Chesskell.Commons 
    ( Tag (..)
    , Color (..)
    , FigureType (..)
    ) where

data Tag = Tag 
    { tagName  :: String
    , tagValue :: String 
    } deriving (Eq, Show, Read)

data Color = White | Black 
    deriving (Eq, Show, Read)

data FigureType = King | Queen | Rook | Knight | Bishop | Pawn 
    deriving (Eq, Show, Read)