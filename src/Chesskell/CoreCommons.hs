{-# LANGUAGE TemplateHaskell #-}

module Chesskell.CoreCommons
  ( Tag (..)
  , Move (..)
  , Game (..)
  , AppState (..)
  , staticPic
  , pieceToPicMap
  , games
  , gameNum
  , moveNum
  , supportedTagNames
  , getCurrentGame
  , getCurrentMove
  ) where

import           Data.Vector     (Vector)
import qualified Data.Vector     as Vector
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.Map        (Map)
import           Control.Lens    (makeLenses, (^.))
import qualified Graphics.Gloss  as UI
import           Chesskell.Chess (Color, Piece, Position, Arrangement)

data Tag = Tag
  { tagName  :: String
  , tagValue :: String
  } deriving (Show)

data Move = Move
  { fromPos       :: Position
  , toPos         :: Position
  , pieceToPosMap :: Map Piece [Position]
  } deriving (Show)

data Game = Game
  { tags   :: [Tag]
  , moves  :: Vector Move
  , winner :: Maybe Color
  } deriving (Show)

data AppState = AppState
  { _staticPic     :: UI.Picture
  , _pieceToPicMap :: Map Piece UI.Picture
  , _games         :: Vector Game
  , _gameNum       :: Int
  , _moveNum       :: Int
  } deriving (Show)
  
makeLenses ''AppState

supportedTagNames :: Set String
supportedTagNames = Set.fromList ["Event", "Site", "Date", "Round", "White", "Black", "Result"]

getCurrentGame :: AppState -> Game
getCurrentGame appState = (appState^.games) Vector.! (appState^.gameNum)

getCurrentMove :: AppState -> Move
getCurrentMove appState =
  let game = getCurrentGame appState
  in moves game Vector.! (appState^.moveNum)