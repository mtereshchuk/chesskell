module Chesskell.Module where

import Graphics.Gloss
import Chesskell.Chess
import Chesskell.Game
import Chesskell.PGNParser
import Chesskell.Preprocessor
import System.Environment
import qualified Data.Matrix as Matrix
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

data World = World
    { games   :: [Game]
    , gameNum :: Int
    , moveNum :: Int
    , figurePicMap :: Map Piece Picture
    }

getDisplay :: Display
getDisplay = InWindow "Chesskell" (700, 700) (0, 0)

run :: IO ()
run = putStrLn "hello" {-do
    args <- getArgs
    let filePath = args !! 0
    res <- parsePGNFile filePath
    let (Right rawGames) = res
    let games = preprocessRawGames rawGames
    let figs = getAllPieces :: [Piece]
    let a = map figureToPicture figs
    pics <- sequence a
    let pairList = zip figs pics
    let map = Map.fromList pairList
    let world = World games 0 0 map
    play getDisplay (greyN 0.5) 3 world undefined undefined undefined

showWorld :: World -> Picture
showWorld (World games gameNum moveNum figureToPicMap) =
    let game = games !! gameNum
        move = (arrangements game) !! moveNum
        argm = moveArrangement move
    in undefined

figureImagePath :: Piece -> FilePath
figureImagePath (color, pieceType) = "resources/images/" ++ show color ++ "_" ++ show pieceType ++ ".bmp"

figureToPicture :: Piece -> IO Picture
figureToPicture figure = do
    figPic <- loadBMP (figureImagePath figure)
    --let x = (fromIntegral j) * 80.0 - 320.0
      --  y = (fromIntegral i) * (-80.0) + 320.0
    let a = {-translate x y-} (scale 0.15 0.15 figPic)
    return a

cellPicture :: Position -> Picture
cellPicture (i, j) = 
    let col = if (even $ i+ j) then white else (greyN 0.3)
        x = (fromIntegral j) * 80.0 - 320.0
        y = (fromIntegral i) * (-80.0) + 320.0
    in translate x y (color col (rectangleSolid 80.0 80.0))

placeToPicture :: Position -> Place -> Picture
placeToPicture pos Nothing       = cellPicture pos
placeToPicture pos (Just figure) = 
    let cellPic = cellPicture pos
        figurePic = figureToPicture figure
    in pictures [cellPic, undefined]

arrangementToPic :: Arrangement -> Picture
arrangementToPic arngm = --do
    let pics = fmap func [(x, y) | x <- [1..chessBoardLength], y <- [1..chessBoardLength]]
    --a <- sequence pics
    return (pictures undefined)
    where func pos = placeToPicture pos (arngm Matrix.! pos)-}