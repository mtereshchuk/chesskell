{-# LANGUAGE TemplateHaskell #-}

module Chesskell.Converter where

import Prelude hiding (round)
import Data.List      (find, iterate)
import Data.Char
import Data.Matrix
import Control.Lens
import Chesskell.ChessCommons
import Chesskell.PGNParser

data PositionMap = PositionMap
    { _queens  :: [Position]
    , _rooks   :: [Position]
    , _knights :: [Position]
    , _bishops :: [Position]
    , _pawns   :: [Position]
    }

data GameState = GameState
    { _arrangement      :: Arrangement
    , _whitePositionMap :: PositionMap
    , _blackPositionMap :: PositionMap
    }

type ExtraCoord = Maybe Int

makeLenses ''PositionMap
makeLenses ''GameState

convertRawTags :: [RawTag] -> Tags
convertRawTags rawTags = 
    let findTagValue name = tagValue <$> find (\t -> tagName t == name) rawTags
    in Tags 
        { event  = findTagValue "Event"
        , site   = findTagValue "Site"
        , date   = findTagValue "Date"
        , round  = findTagValue "Round"
        , white  = findTagValue "White"
        , black  = findTagValue "Black"
        , result = findTagValue "Result"
        }

convertX :: X -> Int
convertX (X letter) = ord letter - ord 'a'

convertY :: Y -> Int
convertY (Y digit) = 8 - digit

convertRawExtraCoord :: RawExtraCoord -> ExtraCoord
convertRawExtraCoord Nothing          = Nothing
convertRawExtraCoord (Just (Left x))  = Just $ convertX x
convertRawExtraCoord (Just (Right y)) = Just $ convertY y

convertRawPosition :: RawPosition -> Position
convertRawPosition (x, y) = (convertX x, convertY y)

convertRawMove :: RawMove -> Color -> GameState -> Move
convertRawMove (BaseRawMove figureType rawExtraCoord wasCapture rawPosition turnIntoType _ _) color gameState
    | figureType == King   = undefined
    | figureType == Queen  = undefined
    | figureType == Rook   = undefined
    | figureType == Knight = undefined
    | figureType == Bishop = undefined
    | otherwise            = undefined
    where extraCoord = convertRawExtraCoord rawExtraCoord
          position   = convertRawPosition rawPosition

calcNextGameState :: Move -> GameState -> GameState
calcNextGameState (BaseMove figure fromPos toPos) gameState = 
    arrangement %~ (cleanFromPos . fillToPos) $ gameState
    where cleanFromPos = setElem Nothing fromPos
          fillToPos    = setElem (Just figure) toPos
calcNextGameState (EnPassant color fromPos toPos capturedPawnPos) gameState = 
    arrangement %~ (cleanFromPos . fillToPos . cleanCapturedPos) $ gameState
    where cleanFromPos     = setElem Nothing fromPos
          fillToPos        = setElem (Just (color, Pawn)) toPos 
          cleanCapturedPos = setElem Nothing capturedPawnPos
calcNextGameState (Castling color kingFromPos kingToPos rookFromPos rookToPos) gameState = 
    arrangement %~ (cleanKingFromPos . fillKingToPos . cleanRookFromPos . fillRookToPos) $ gameState
    where cleanKingFromPos = setElem Nothing kingFromPos
          fillKingToPos    = setElem (Just (color, King)) kingToPos 
          cleanRookFromPos = setElem Nothing rookFromPos
          fillRookToPos    = setElem (Just (color, Rook)) rookToPos 
          
convertRawMovesHelper :: [RawMove] -> [Move] -> Color -> GameState -> [Move]
convertRawMovesHelper [] moves _ _                   = moves
convertRawMovesHelper (rm : rms) moves color gameState = 
    let move          = convertRawMove rm color gameState
        nextColor     = oppositeColor color
        nextGameState = calcNextGameState move gameState
    in convertRawMovesHelper rms (moves ++ [move]) nextColor nextGameState

initialGameState :: GameState
initialGameState = GameState
    { _arrangement      = initialArrangement
    , _whitePositionMap = PositionMap 
        { _queens  = [(7, 3)]
        , _rooks   = [(7, 2), (7, 5)]
        , _knights = [(7, 1), (7, 6)]
        , _bishops = [(7, 0), (7, 7)]
        , _pawns   = take 8 $ iterate (over _2 (+1)) (6, 0)
        }
    , _blackPositionMap = PositionMap 
        { _queens  = [(0, 3)]
        , _rooks   = [(0, 2), (0, 5)]
        , _knights = [(0, 1), (0, 6)]
        , _bishops = [(0, 0), (0, 7)]
        , _pawns   = take 8 $ iterate (over _2 (+1)) (1, 0)
        }  
    }

convertRawMoves :: [RawMove] -> [Move]
convertRawMoves rawMoves = convertRawMovesHelper rawMoves [] White initialGameState

convertRawGame :: RawGame -> Game
convertRawGame rawGame = Game 
        { tags   = convertRawTags  $ rawTags  rawGame
        , moves  = convertRawMoves $ rawMoves rawGame
        , winner = rawWinner rawGame
        }