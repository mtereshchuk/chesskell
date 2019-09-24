{-# LANGUAGE TupleSections #-}

module Chesskell.View
 ( chesskellDisplay
 , getStaticPic
 , getPieceToPicMap
 , appStateToPic
 ) where

import           Data.List             (intercalate)
import           Data.Map              (Map)
import qualified Data.Map              as Map
import qualified Data.Vector           as Vector
import           Control.Lens          ((^.))
import qualified Graphics.Gloss        as UI
import           Chesskell.Chess       (Piece, Position, boardLength, getAllPieces)
import           Chesskell.CoreCommons (Tag (..), Move (..), Game (..), AppState, 
                                       staticPic, pieceToPicMap, getCurrentGame, getCurrentMove)

viewScale :: Float
viewScale = 60.0

chesskellDisplay :: UI.Display
chesskellDisplay = UI.InWindow windowName windowSize windowPos
  where
    windowName = "Chesskell"
    windowSize =
      let viewScaleInt = round viewScale
      in (viewScaleInt * 13, viewScaleInt * 10)
    windowPos  = (0, 0)

backgroundColor :: UI.Color
backgroundColor = UI.white

constructSquare :: Float -> UI.Color -> Float -> Float -> UI.Picture
constructSquare width sqColor xShift yShift =
  UI.translate xShift yShift
  $ UI.color sqColor
  $ UI.rectangleSolid width width

boardXShift :: Float
boardXShift = viewScale * (-2.0)

boardYShift :: Float
boardYShift = 0.0

getPosShifts :: Position -> (Float, Float)
getPosShifts (i, j) =
  let xShift = fromIntegral j * baseXShift - baseYShift + boardXShift
      yShift = fromIntegral i * (-baseXShift) + baseYShift + boardYShift
  in (xShift, yShift)
  where
    baseXShift = viewScale
    baseYShift = viewScale * 4.5

fieldToPic :: Position -> UI.Picture
fieldToPic pos@(i, j) =
  let fieldColor       =
        if even $ i + j
        then whiteFieldColor
        else blackFieldColor
      (xShift, yShift) = getPosShifts pos
  in constructSquare fieldWidth fieldColor xShift yShift
  where
    fieldWidth      = viewScale
    whiteFieldColor = UI.white
    blackFieldColor = UI.greyN 0.3

getBoardPic :: UI.Picture
getBoardPic =
  let boardBack = UI.translate boardXShift boardYShift $ constructSquare boardBackWidth boardBackColor 0.0 0.0
      fieldsPic = UI.pictures $ map fieldToPic [(i, j) | i <- [1..boardLength], j <- [1..boardLength]]
  in UI.pictures [boardBack, fieldsPic]
  where
    boardBackWidth = (viewScale * fromIntegral boardLength) * 1.025
    boardBackColor = UI.black

getInfoPlacePic :: UI.Picture
getInfoPlacePic = constructSquare infoSquareWidth infoSquareColor xShift yShift
  where
    infoSquareWidth = viewScale * 3.5
    infoSquareColor = UI.greyN 0.95
    xShift          = viewScale * 4.3
    yShift          = viewScale * 2.35

getControlPic :: UI.Picture
getControlPic =
  let controlPics = map UI.text [prevGame, nextGame, prevMove, nextMove]
      picModify i = UI.translate 0.0 (betweenShift * fromIntegral i) (UI.scale controlScale controlScale $ controlPics !! i)
      controlList = [picModify i | i <- [0..length controlPics - 1]]
  in UI.translate xShift yShift $ UI.pictures controlList
  where
    nextMove     = "Next Move: Right Arrow"
    prevMove     = "Prev Move: Left Arrow"
    nextGame     = "Next Game: Ctrl + Right Arrow"
    prevGame     = "Prev Game: Ctrl + Left Arrow"
    controlScale = viewScale / 605.0
    betweenShift = viewScale / 2.2
    xShift       = viewScale * 2.55
    yShift       = -viewScale * 1.5

getStaticPic :: UI.Picture
getStaticPic = UI.pictures [getBoardPic, getInfoPlacePic, getControlPic]

getPieceBMPPath :: Piece -> FilePath
getPieceBMPPath (color, pieceType) =
  let commonPrefix  = "resources/images/"
      colorName     = show color
      separator     = "_"
      pieceTypeName = show pieceType
      fileExtension = ".bmp"
  in concat [commonPrefix, colorName, separator, pieceTypeName, fileExtension]

pieceToPic :: Piece -> IO UI.Picture
pieceToPic piece = do
  piecePic <- UI.loadBMP $ getPieceBMPPath piece
  return $ UI.scale pieceScale pieceScale piecePic
  where
    pieceScale = viewScale * 0.15

getPieceToPicMap :: IO (Map Piece UI.Picture)
getPieceToPicMap = do
  let pieces        = getAllPieces
      piecePicsIO   = map pieceToPic pieces
  piecePics         <- sequence piecePicsIO
  let pieceToPicMap = Map.fromList $ zip pieces piecePics
  return pieceToPicMap

getInfoPic :: [Tag] -> UI.Picture
getInfoPic tags =
  let mergedTags  = map (\(Tag name value) -> name ++ ": " ++ value) tags
      tagPics     = map UI.text mergedTags
      picModify i = UI.translate 0.0 (betweenShift * fromIntegral i) (UI.scale tagsScale tagsScale $ tagPics !! i)
      tagList     = [picModify i | i <- [0..length tagPics - 1]] -- copypaste
  in UI.translate xShift yShift $ UI.pictures tagList
  where
    tagsScale    = viewScale / 605.0
    betweenShift = viewScale / 2.2
    xShift       = viewScale * 2.55
    yShift       = viewScale * 2.5

getMovePositionsPic :: Position -> Position -> UI.Picture
getMovePositionsPic fromPos toPos =
  let posToPic pos posColor = UI.translate boardYShift boardYShift $ UI.color posColor $ fieldToPic pos
      fromPosPic            = posToPic fromPos fromPosColor
      toPosPic              = posToPic toPos toPosColor
  in UI.pictures [fromPosPic, toPosPic]
  where
    fromPosColor = UI.green
    toPosColor   = UI.green

getPiecesPic :: Map Piece UI.Picture -> Map Piece [Position] -> UI.Picture
getPiecesPic pieceToPicMap pieceToPosMap =
  let toPairList (piece, positions) = map (piece,) positions
      pieceAndPosList               = concatMap toPairList $ Map.toList pieceToPosMap
      pieceAndPosToPic (piece, pos) =
        let piecePic = pieceToPicMap Map.! piece
            (xShift, yShift) = getPosShifts pos
        in UI.translate xShift yShift piecePic
  in UI.pictures $ map pieceAndPosToPic pieceAndPosList

getArrangementPic :: Map Piece UI.Picture -> Move -> UI.Picture
getArrangementPic pieceToPicMap (Move fromPos toPos pieceToPosMap) =
  let movePositionsPic = getMovePositionsPic fromPos toPos
      piecesPic        = getPiecesPic pieceToPicMap pieceToPosMap
  in UI.pictures [movePositionsPic, piecesPic]

appStateToPic :: AppState -> UI.Picture
appStateToPic appState =
  let move           = getCurrentMove appState
      infoPic        = getInfoPic $ tags $ getCurrentGame appState
      arrangementPic = getArrangementPic (appState^.pieceToPicMap) move
  in UI.pictures [appState^.staticPic, infoPic, arrangementPic]