module Main where

import System.Environment            (getArgs)
import Text.ParserCombinators.Parsec (parseFromFile, parse)
import Chesskell.PGNParser
import Chesskell.ChessCommons
import Chesskell.Preprocessor
import Data.List

main = do
    args <- getArgs
    let filePath = args !! 0
    (Right rawGames) <- parsePGNFile filePath
    let argms = arrangements (head $ preprocessRawGames rawGames)
    putStrLn $ intercalate "\n\n" (map arrangementToString argms)