module Chesskell.View
 ( chesskellDisplay
 , getStaticPic
 , getPieceToPicMap
 , stateToPic
 ) where

import Data.List (intercalate)
import Data.Map as Map (Map)
import qualified Data.Map as Map
--import qualified Graphics.Gloss as Gloss
import qualified Data.Vector as Vector
import Graphics.Gloss
import Chesskell.Commons hiding (round)
import Chesskell.Chess hiding (Color)

viewScale :: Float
viewScale = 60.0

chesskellDisplay :: Display
chesskellDisplay = InWindow windowName windowSize windowPos
  where
    windowName = "Chesskell"
    windowSize =
      let viewScaleInt = round viewScale
      in (viewScaleInt * 13, viewScaleInt * 10)
    windowPos  = (0, 0)

constructSquare :: Float -> Color -> Float -> Float -> Picture
constructSquare width sqColor xShift yShift =
  translate xShift yShift
  $ color sqColor
  $ rectangleSolid width width

fieldToPic :: Position -> Picture
fieldToPic (i, j) =
  let fieldColor = if even $ i + j
        then whiteFieldColor
        else blackFieldColor
      xShift     = fromIntegral j * baseXShift - baseYShift
      yShift     = fromIntegral i * (-baseXShift) + baseYShift
  in constructSquare fieldWidth fieldColor xShift yShift
  where
    fieldWidth      = viewScale
    whiteFieldColor = white
    blackFieldColor = greyN 0.3
    baseXShift      = fieldWidth
    baseYShift      = fieldWidth * 4.5

boardXShift :: Float
boardXShift = viewScale * (-2.0)

boardYShift :: Float
boardYShift = 0.0

getBoardPic :: Picture
getBoardPic =
  let boardBack = constructSquare boardBackWidth boardBackColor 0.0 0.0
      fieldsPic = pictures $ map fieldToPic [(i, j) | i <- [1..boardLength], j <- [1..boardLength]]
  in translate boardXShift boardYShift $ pictures [boardBack, fieldsPic]
  where
    boardBackWidth = (viewScale * fromIntegral boardLength) * 1.025
    boardBackColor = black

getInfoPlacePic :: Picture
getInfoPlacePic = constructSquare infoSquareWidth infoSquareColor xShift yShift
  where
    infoSquareWidth = viewScale * 3.5
    infoSquareColor = greyN 0.95
    xShift          = viewScale * 4.3
    yShift          = viewScale * 2.35

getControlPic :: Picture
getControlPic =
  let controlPics = map text [prevGame, nextGame, prevMove, nextMove]
      picModify i = translate 0.0 (betweenShift * fromIntegral i) (scale controlScale controlScale $ controlPics !! i)
      controlList = [picModify i | i <- [0..length controlPics - 1]]
  in translate xShift yShift $ pictures controlList
  where
    nextMove     = "Next Move: Right Arrow"
    prevMove     = "Prev Move: Left Arrow"
    nextGame     = "Next Game: Ctrl + Right Arrow"
    prevGame     = "Prev Game: Ctrl + Left Arrow"
    controlScale = viewScale / 605.0
    betweenShift = viewScale / 2.2
    xShift       = viewScale * 2.55
    yShift       = -viewScale * 1.5

getStaticPic :: Picture
getStaticPic = pictures [getBoardPic, getInfoPlacePic, getControlPic]

getPieceBMPPath :: Piece -> FilePath
getPieceBMPPath (color, pieceType) =
  let commonPrefix  = "resources/images/"
      colorName     = show color
      separator     = "_"
      pieceTypeName = show pieceType
      fileExtension = ".bmp"
  in concat [commonPrefix, colorName, separator, pieceTypeName, fileExtension]

pieceToPic :: Piece -> IO Picture
pieceToPic piece = do
  piecePic <- loadBMP $ getPieceBMPPath piece
  return $ scale pieceScale pieceScale piecePic
  where
    pieceScale = viewScale * 0.15

getPieceToPicMap :: IO (Map Piece Picture)
getPieceToPicMap = do
  let pieces        = getAllPieces
      piecePicsIO   = map pieceToPic pieces
  piecePics         <- sequence piecePicsIO
  let pieceToPicMap = Map.fromList $ zip pieces piecePics
  return pieceToPicMap

getInfoPic :: [Tag] -> Picture
getInfoPic tags =
  let mergedTags  = map (\(Tag name value) -> name ++ ": " ++ value) tags
      tagPics     = map text mergedTags
      picModify i = translate 0.0 (betweenShift * fromIntegral i) (scale tagsScale tagsScale $ tagPics !! i)
      tagList     = [picModify i | i <- [0..length tagPics - 1]] -- copypaste
  in translate xShift yShift $ pictures tagList
  where
    tagsScale    = viewScale / 605.0
    betweenShift = viewScale / 2.2
    xShift       = viewScale * 2.55
    yShift       = viewScale * 2.5

getArrangementPic :: Map Piece Picture -> Move -> Picture
getArrangementPic a b = blank -- translate boardXShift boardYShift $ arrangePieces pieceToPicMap pieceToPosMap

stateToPic :: State -> Picture
stateToPic (State staticPic pieceToPicMap games gameNum moveNum) =
  let (Game tags moves winner) = games Vector.! gameNum
      move                     = moves Vector.! moveNum
      infoPic                  = getInfoPic tags
      arrangementPic           = getArrangementPic pieceToPicMap move
  in pictures [staticPic, infoPic, arrangementPic]