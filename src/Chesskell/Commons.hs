module Chesskell.Commons
  ( Tag (..)
  , Move (..)
  , Game (..)
  , State (..)
  , supportedTagNames
  ) where

import           Data.Vector     (Vector)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.Map        (Map)
import           Graphics.Gloss  (Picture)
import           Chesskell.Chess (Color, Piece, Position, Arrangement)

data Tag = Tag
  { tagName  :: String
  , tagValue :: String
  }

data Move = Move
  { fromPos       :: Position
  , toPos         :: Position
  , pieceToPosMap :: Map Piece [Position]
  }

data Game = Game
  { tags   :: [Tag]
  , moves  :: Vector Move
  , winner :: Maybe Color
  }

data State = State
  { staticPic     :: Picture
  , pieceToPicMap :: Map Piece Picture
  , games         :: Vector Game
  , gameNum       :: Int
  , moveNum       :: Int
  }
  
supportedTagNames :: Set String
supportedTagNames = Set.fromList ["Event", "Site", "Date", "Round", "White", "Black", "Result"]