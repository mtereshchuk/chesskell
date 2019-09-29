module Chesskell.ControlTest
  ( controlTest
  ) where

import           Data.Vector                          (Vector)
import qualified Data.Vector                          as Vector
import qualified Data.Map                             as Map
import           Control.Lens                         ((^.), (.~))
import           Test.Tasty                           (TestTree)
import           Test.Tasty.Hspec                     (Spec, testSpec, describe, it, shouldSatisfy)
import qualified Graphics.Gloss.Interface.IO.Interact as UI
import           Chesskell.CoreCommons                (Move (..), Game (..), AppState (..), gameNum, moveNum)
import           Chesskell.Control                    (updateAppState)

specControl :: Spec
specControl = do
  describe "Prev move works" $ do
    it "First move change" $
      updateAppState prevMoveEvent firstMoveAppState 
        `shouldSatisfy` (\appState -> appState^.moveNum == firstMoveAppState^.moveNum)
    it "Middle move change" $
      updateAppState prevMoveEvent middleMoveAppState 
        `shouldSatisfy` (\appState -> appState^.moveNum == middleMoveAppState^.moveNum - 1)
  describe "Next move works" $ do
    it "Last move change" $
      updateAppState nextMoveEvent lastMoveAppState 
        `shouldSatisfy` (\appState -> appState^.moveNum == lastMoveAppState^.moveNum)
    it "Middle move change" $
      updateAppState nextMoveEvent middleMoveAppState 
        `shouldSatisfy` (\appState -> appState^.moveNum == middleMoveAppState^.moveNum + 1)
  describe "Prev game works" $ do
    it "First game change" $
      updateAppState prevGameEvent firstGameAppState 
        `shouldSatisfy` (\appState -> appState^.gameNum == firstGameAppState^.gameNum)
    it "Middle game change" $
      updateAppState prevGameEvent middleGameAppState 
        `shouldSatisfy` (\appState -> appState^.gameNum == middleGameAppState^.gameNum - 1)
  describe "Next game works" $ do
    it "Last game change" $
      updateAppState nextGameEvent lastGameAppState 
        `shouldSatisfy` (\appState -> appState^.gameNum == lastGameAppState^.gameNum)
    it "Middle game change" $
      updateAppState nextGameEvent middleGameAppState 
        `shouldSatisfy` (\appState -> appState^.gameNum == middleGameAppState^.gameNum + 1)
  where
    prevMoveEvent = UI.EventKey
      (UI.SpecialKey UI.KeyLeft)
       UI.Down
      (UI.Modifiers UI.Up UI.Up UI.Up)
      (0.0, 0.0)
    nextMoveEvent = UI.EventKey
      (UI.SpecialKey UI.KeyRight)
       UI.Down
      (UI.Modifiers UI.Up UI.Up UI.Up)
      (0.0, 0.0)
    prevGameEvent = UI.EventKey
      (UI.SpecialKey UI.KeyLeft)
       UI.Down
      (UI.Modifiers UI.Down UI.Up UI.Up)
      (0.0, 0.0)
    nextGameEvent = UI.EventKey
      (UI.SpecialKey UI.KeyRight)
       UI.Down
      (UI.Modifiers UI.Down UI.Up UI.Up)
      (0.0, 0.0)
    baseMove           = Move
      { fromPos       = (0, 0)
      , toPos         = (0, 0)
      , pieceToPosMap = Map.empty
      }
    baseGame           = Game
      { tags   = []
      , moves  = Vector.fromList $ replicate 3 baseMove
      , winner = Nothing
      }
    baseAppState       = AppState
      { _staticPic     = UI.blank
      , _pieceToPicMap = Map.empty
      , _games         = Vector.fromList $ replicate 3 baseGame
      , _gameNum       = 0
      , _moveNum       = 0
      }
    firstMoveAppState  = moveNum .~ 0 $ baseAppState
    middleMoveAppState = moveNum .~ 1 $ baseAppState
    lastMoveAppState   = moveNum .~ 2 $ baseAppState
    firstGameAppState  = gameNum .~ 0 $ baseAppState
    middleGameAppState = gameNum .~ 1 $ baseAppState
    lastGameAppState   = gameNum .~ 2 $ baseAppState
    
controlTest :: IO TestTree
controlTest = testSpec "Control" specControl