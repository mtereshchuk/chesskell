module Chesskell.Module
  ( run
  ) where

import           System.Environment     (getArgs)
import           Data.Vector            (Vector)
import qualified Graphics.Gloss         as UI
import           Chesskell.Chess        (Piece)
import           Chesskell.CoreCommons  (Game, AppState (..))
import           Chesskell.PGNParser    (parsePGNFile)
import           Chesskell.Preprocessor (preprocess)
import           Chesskell.View         (chesskellDisplay, backgroundColor, getStaticPic, getPieceToPicMap, appStateToPic)
import           Chesskell.Control      (updateAppState)

process :: IO (Either String AppState)
process = do
  args <- getArgs
  if null args
  then return $ Left missingFilePath
  else do
    let filePath = head args
    parseRes     <- parsePGNFile filePath
    case parseRes of
      (Left parseErrorMsg) -> return . Left $ parseError ++ show parseErrorMsg
      (Right rawGames)     -> do
        let preprocessRes = preprocess rawGames
        case preprocessRes of
          (Left preprocessErrorMsg) -> return . Left $ preprocessError ++ preprocessErrorMsg
          (Right games)             -> do
            staticPic     <- getStaticPic
            pieceToPicMap <- getPieceToPicMap
            return $ Right AppState
              { _staticPic     = staticPic
              , _pieceToPicMap = pieceToPicMap
              , _games         = games
              , _gameNum       = 0
              , _moveNum       = 1
              }
  where
    missingFilePath = "Missing file path"
    parseError      = "Error during parsing file: "
    preprocessError = "Error during processing file: "

run :: IO ()
run = do
  processRes <- process
  case processRes of
    (Left errorMsg)         -> putStrLn errorMsg 
    (Right initialAppState) -> 
      UI.play
        chesskellDisplay
        backgroundColor
        0
        initialAppState
        appStateToPic
        updateAppState
        (const id)