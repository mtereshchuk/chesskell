module Chesskell.PGNParser
    ( RawTag (..)
    , RawMove (..)
    , RawGame (..)
    , X (..)
    , Y (..)
    , ExtraCoord
    , RawPosition
    , rawGameParser
    , rawGamesParser
    ) where

import Control.Applicative           ((<|>))
import Data.Char                     (digitToInt)
import Text.Parsec                   (try)
import Text.ParserCombinators.Parsec (Parser, char, string, letter, digit, 
                                      spaces,  many, oneOf, noneOf)
import Chesskell.ChessCommons        (Color (..), FigureType (..))

data RawTag = RawTag 
    { tagName  :: String
    , tagValue :: String 
    } deriving (Eq, Show, Read)

data RawMove
    = BaseRawMove
    { figureType   :: FigureType
    , extraCoord   :: ExtraCoord
    , wasCapture   :: Bool
    , destPosition :: RawPosition
    , turnIntoType :: Maybe FigureType
    , wasCheck     :: Bool
    , wasMate      :: Bool
    } 
    | ShortCastling
    { wasCheck     :: Bool
    , wasMate      :: Bool
    } 
    | LongCastling
    { wasCheck     :: Bool
    , wasMate      :: Bool
    } deriving (Eq, Show, Read)

data RawGame = RawGame
    { rawTags  :: [RawTag]
    , rawMoves :: [RawMove]
    , winner   :: Maybe Color
    } deriving (Eq, Show, Read)

newtype X = X Char deriving (Eq, Show, Read)
newtype Y = Y Int deriving (Eq, Show, Read)

type ExtraCoord = Maybe (Either X Y)
type RawPosition = (X, Y)

rawTagParser :: Parser RawTag
rawTagParser = do
    char '['
    tagName <- many letter
    spaces
    tagValueWithQuotes <- many $ noneOf "]"
    let tagValue = init $ tail tagValueWithQuotes
    char ']'
    return RawTag
        { tagName  = tagName
        , tagValue = tagValue
        }

numberParser :: Parser Int
numberParser = read <$> many digit

figureTypePawnExParser :: Parser FigureType
figureTypePawnExParser = 
        King   <$ char 'K'
    <|> Queen  <$ char 'Q'
    <|> Rook   <$ char 'R'
    <|> Knight <$ char 'N'
    <|> Bishop <$ char 'B'

figureTypeParser :: Parser FigureType
figureTypeParser = figureTypePawnExParser <|> pure Pawn

xParser :: Parser X
xParser = X <$> oneOf "abcdefgh"
                         
yParser :: Parser Y
yParser = Y . digitToInt <$> oneOf "12345678"

extraCoordParser :: Parser (Maybe (Either X Y))
extraCoordParser =      
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

turnIntoTypeParser :: Parser (Maybe FigureType)
turnIntoTypeParser = 
        Just <$> (char '=' *> figureTypePawnExParser)
    <|> Just <$> figureTypePawnExParser
    <|> pure Nothing

mainPartOfBaseRawMoveParser :: Parser (ExtraCoord, Bool, RawPosition)
mainPartOfBaseRawMoveParser = try case1 <|> case2
    where 
        case1 = do
            extraCoord  <- extraCoordParser
            wasCapture  <- wasCaptureParser
            rawPosition <- rawPositionParser
            return (extraCoord, wasCapture, rawPosition)
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
    figureType                             <- figureTypeParser
    (extraCoord, wasCapture, destPosition) <- mainPartOfBaseRawMoveParser
    turnIntoType                           <- turnIntoTypeParser
    wasCheck                               <- wasCheckParser
    wasMate                                <- wasMateParser
    return BaseRawMove 
        { figureType   = figureType
        , extraCoord   = extraCoord
        , wasCapture   = wasCapture
        , destPosition = destPosition
        , turnIntoType = turnIntoType
        , wasCheck     = wasCheck
        , wasMate      = wasMate
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

winnerParser :: Parser (Maybe Color)
winnerParser = 
    try (Just <$> (White <$ string "1-0"))
    <|>  Just <$> (Black <$ string "0-1")
    <|>  Nothing <$ string "1/2-1/2"

rawGameParser :: Parser RawGame
rawGameParser = do
    rawTags  <- many $ rawTagParser <* spaces
    rawMoves <- rawMovesParser
    winner   <- winnerParser
    return RawGame
        { rawTags  = rawTags
        , rawMoves = rawMoves 
        , winner   = winner
        }

rawGamesParser :: Parser [RawGame]
rawGamesParser = many $ rawGameParser <* spaces