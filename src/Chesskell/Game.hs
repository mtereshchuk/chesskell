module Chesskell.Game
  ( Tags (..)
  , Move (..)
  , Game (..)
  ) where

import Chesskell.Chess (Color, Position, Arrangement) 

data Tags = Tags
  { event  :: Maybe String
  , site   :: Maybe String
  , date   :: Maybe String
  , round  :: Maybe String
  , whiteP :: Maybe String
  , blackP :: Maybe String
  , result :: Maybe String
  }

data Move = Move
 { arrangement :: Arrangement
 , fromPos     :: Position
 , toPos       :: Position
 }

data Game = Game
  { tags         :: Tags
  , arrangements :: [Move]
  , winner       :: Maybe Color
  }