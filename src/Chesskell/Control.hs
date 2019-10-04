module Chesskell.Control
  ( updateAppState
  ) where

import qualified Data.Vector                          as Vector
import           Control.Lens                         ((^.), (.~), (%~))
import           Graphics.Gloss.Interface.IO.Interact as UI
import           Chesskell.CoreCommons                (AppState, games, moves, gameNum, moveNum, getCurrentGame)

updateAppState :: UI.Event -> AppState -> AppState

updateAppState
  ( UI.EventKey
      (UI.SpecialKey UI.KeyLeft)
       UI.Down
      (UI.Modifiers UI.Up UI.Up UI.Up)
       _
  )
  appState =
    if appState^.moveNum == 0
    then appState
    else moveNum %~ (\num -> num - 1) $ appState

updateAppState
  (UI.EventKey
    (UI.SpecialKey UI.KeyRight)
     UI.Down
    (UI.Modifiers UI.Up UI.Up UI.Up)
     _
  )
  appState =
    let game     = getCurrentGame appState
        movesNum = Vector.length $ moves game
    in if appState^.moveNum == movesNum - 1
       then appState
       else moveNum %~ (+1) $ appState

updateAppState
  (UI.EventKey
    (UI.SpecialKey UI.KeyLeft)
     UI.Down
    (UI.Modifiers UI.Down UI.Up UI.Up)
     _
  )
  appState =
    if appState^.gameNum == 0
    then appState
    else moveNum .~ 1 $ (gameNum %~ (\num -> num - 1) $ appState)

updateAppState 
  (UI.EventKey 
    (UI.SpecialKey UI.KeyRight) 
     UI.Down 
    (UI.Modifiers UI.Down UI.Up UI.Up) 
     _
  ) 
  appState =
    let gamesNum = Vector.length $ appState^.games
    in if appState^.gameNum == gamesNum - 1
       then appState
       else moveNum .~ 1 $ gameNum %~ (+1) $ appState

updateAppState _ appState = appState