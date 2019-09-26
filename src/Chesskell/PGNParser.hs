module Chesskell.PGNParser
  ( RawTag (..)
  , RawMove (..)
  , RawGame (..)
  , X (..)
  , Y (..)
  , RawExtraCoord
  , RawPosition
  , parsePGNFile
  ) where

import Data.Char                     (digitToInt)
import Control.Applicative           ((<|>))
import Text.ParserCombinators.Parsec (Parser, ParseError, char, string, letter, digit, 
                                     spaces, many, oneOf, noneOf, try, parseFromFile)
import Chesskell.Chess               (Color (..), PieceType (..))

data RawTag = RawTag
  { rawTagName  :: String
  , rawTagValue :: String
  } deriving (Eq, Show)

data RawMove
  = BaseRawMove
  { pieceType     :: PieceType
  , rawExtraCoord :: RawExtraCoord
  , wasCapture    :: Bool
  , rawPosition   :: RawPosition
  , turnIntoType  :: Maybe PieceType
  , wasCheck      :: Bool
  , wasMate       :: Bool
  }
  | ShortCastling
  { wasCheck     :: Bool
  , wasMate      :: Bool
  }
  | LongCastling
  { wasCheck     :: Bool
  , wasMate      :: Bool
  } deriving (Eq, Show)

data RawGame = RawGame
  { rawTags   :: [RawTag]
  , rawMoves  :: [RawMove]
  , rawWinner :: Maybe Color
  } deriving (Eq, Show)

newtype X = X Char deriving (Eq, Show)
newtype Y = Y Int deriving (Eq, Show)

type RawExtraCoord = Maybe (Either X Y)
type RawPosition = (X, Y)

rawTagParser :: Parser RawTag
rawTagParser = do
  char '['
  tagName            <- many letter
  spaces
  tagValueWithQuotes <- many $ noneOf "]"
  let tagValue       = init $ tail tagValueWithQuotes
  char ']'
  return RawTag
    { rawTagName  = tagName
    , rawTagValue = tagValue
    }

numberParser :: Parser Int
numberParser = read <$> many digit

pieceTypePawnExParser :: Parser PieceType
pieceTypePawnExParser =
      King   <$ char 'K'
  <|> Queen  <$ char 'Q'
  <|> Bishop <$ char 'B'
  <|> Knight <$ char 'N'
  <|> Rook   <$ char 'R'

pieceTypeParser :: Parser PieceType
pieceTypeParser = pieceTypePawnExParser <|> pure Pawn

xParser :: Parser X
xParser = X <$> oneOf "abcdefgh"
                         
yParser :: Parser Y
yParser = Y . digitToInt <$> oneOf "12345678"

rawExtraCoordParser :: Parser (Maybe (Either X Y))
rawExtraCoordParser =      
      Just . Left  <$> xParser
  <|> Just . Right <$> yParser
  <|> pure Nothing

wasCaptureParser :: Parser Bool
wasCaptureParser = 
  True <$ char 'x'
  <|> pure False

rawPositionParser :: Parser RawPosition
rawPositionParser = do
  x <- xParser
  y <- yParser
  return (x, y)

turnIntoTypeParser :: Parser (Maybe PieceType)
turnIntoTypeParser = 
      Just <$> (char '=' *> pieceTypePawnExParser)
  <|> Just <$> pieceTypePawnExParser
  <|> pure Nothing

mainPartOfBaseRawMoveParser :: Parser (RawExtraCoord, Bool, RawPosition)
mainPartOfBaseRawMoveParser = try case1 <|> case2
  where
    case1 = do
      rawExtraCoord <- rawExtraCoordParser
      wasCapture    <- wasCaptureParser
      rawPosition   <- rawPositionParser
      return (rawExtraCoord, wasCapture, rawPosition)
    case2 = do
      wasCapture  <- wasCaptureParser
      rawPosition <- rawPositionParser
      return (Nothing, wasCapture, rawPosition)

wasCheckParser :: Parser Bool
wasCheckParser = 
  True <$ char '+'
  <|> pure False

wasMateParser :: Parser Bool
wasMateParser = 
  True <$ char '#'
  <|> pure False

baseRawMoveParser :: Parser RawMove
baseRawMoveParser = do
  pieceType                                <- pieceTypeParser
  (rawExtraCoord, wasCapture, rawPosition) <- mainPartOfBaseRawMoveParser
  turnIntoType                             <- turnIntoTypeParser
  wasCheck                                 <- wasCheckParser
  wasMate                                  <- wasMateParser
  return BaseRawMove
    { pieceType     = pieceType
    , rawExtraCoord = rawExtraCoord
    , wasCapture    = wasCapture
    , rawPosition   = rawPosition
    , turnIntoType  = turnIntoType
    , wasCheck      = wasCheck
    , wasMate       = wasMate
    }

shortCastlingParser :: Parser RawMove
shortCastlingParser = do 
  string "O-O"
  wasCheck <- wasCheckParser
  wasMate  <- wasMateParser
  return ShortCastling
    { wasCheck = wasCheck
    , wasMate  = wasMate
    }

longCastlingParser :: Parser RawMove
longCastlingParser = do 
  string "O-O-O"
  wasCheck <- wasCheckParser
  wasMate  <- wasMateParser
  return LongCastling
    { wasCheck = wasCheck
    , wasMate  = wasMate
    }

rawMoveParser :: Parser RawMove
rawMoveParser = 
  baseRawMoveParser
  <|> try longCastlingParser
  <|> shortCastlingParser

rawMovesParser :: Parser [RawMove]
rawMovesParser = many $
  try (numberParser *> char '.' *> spaces *> rawMoveParser <* spaces)
  <|> rawMoveParser <* spaces

rawWinnerParser :: Parser (Maybe Color)
rawWinnerParser = 
  try (Just <$> (White <$ string "1-0"))
  <|>  Just <$> (Black <$ string "0-1")
  <|>  Nothing <$ string "1/2-1/2"

rawGameParser :: Parser RawGame
rawGameParser = do
  rawTags   <- many $ rawTagParser <* spaces
  rawMoves  <- rawMovesParser
  rawWinner <- rawWinnerParser
  return RawGame
    { rawTags   = rawTags
    , rawMoves  = rawMoves
    , rawWinner = rawWinner
    }

rawGamesParser :: Parser [RawGame]
rawGamesParser = many $ rawGameParser <* spaces

parsePGNFile :: FilePath -> IO (Either ParseError [RawGame])
parsePGNFile = parseFromFile rawGamesParser