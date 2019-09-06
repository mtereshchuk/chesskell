module Chesskell.Converter where

import Chesskell.ChessCommons (Color)
import Chesskell.PGNParser    (RawGame)

data CoreTags = CoreTags
    { event  :: String
    , site   :: String
    , date   :: String
    , round  :: String
    , white  :: String
    , black  :: String
    , result :: String
    } deriving (Eq, Show, Read)

data Move 
    = BaseMove
    { fromPos :: Position
    , toPos   :: Position
    }
    | EnPassant
    { fromPos         :: Position
    , toPos           :: Position
    , capturedPawnPos :: Position
    }
    | Castling
    { kingFromPos :: Position
    , kingToPos   :: Position
    , rookFromPos :: Position
    , rookToPos   :: Position
    }

data Game = Game
    { tags   :: CoreTags
    , moves  :: [Move]
    , winner :: Color
    }

type Position = (Int, Int)

convert :: RawGame -> Game
convert = undefined