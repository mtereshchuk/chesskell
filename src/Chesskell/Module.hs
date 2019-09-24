module Chesskell.Module
  ( run
  ) where

import           System.Environment     (getArgs)
import qualified Graphics.Gloss         as UI
import           Chesskell.Chess        (Piece)
import           Chesskell.CoreCommons  (Game, AppState (..))
import           Chesskell.PGNParser    (parsePGNFile)
import           Chesskell.Preprocessor (preprocessRawGames)
import           Chesskell.View         (chesskellDisplay, backgroundColor, getStaticPic, getPieceToPicMap, appStateToPic)
import           Chesskell.Controller   (updateAppState)

run :: IO ()
run = do
  args <- getArgs
  let filePath = head args
  parseRes <- parsePGNFile filePath
  let (Right rawGames) = parseRes
  let games = preprocessRawGames rawGames
  pieceToPicMap <- getPieceToPicMap
  let initialAppState =
        AppState {_staticPic = getStaticPic, _pieceToPicMap = pieceToPicMap, _games = games, _gameNum = 0, _moveNum = 0}
  UI.play chesskellDisplay backgroundColor 10 initialAppState appStateToPic updateAppState (const id) 