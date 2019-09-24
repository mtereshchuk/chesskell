module Chesskell.Controller where

import qualified Data.Vector as Vector
import Graphics.Gloss.Interface.IO.Interact as UI
import Chesskell.CoreCommons
import Control.Lens

updateAppState :: UI.Event -> AppState -> AppState
updateAppState (UI.EventKey (UI.SpecialKey UI.KeyLeft) UI.Down UI.Modifiers {} _) appState =
  if appState^.moveNum == 0
  then appState
  else moveNum %~ (\num -> num - 1) $ appState
updateAppState (UI.EventKey (UI.SpecialKey UI.KeyRight) UI.Down UI.Modifiers {} _) appState =
  let game = (appState^.games) Vector.! (appState^.gameNum)
  in undefined