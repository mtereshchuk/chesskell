module Main where

import System.Environment            (getArgs)
import Text.ParserCombinators.Parsec (parseFromFile)
import Chesskell.PGNParser           (rawGamesParser)

main = do
    args <- getArgs
    let filePath = args !! 0
    result <- parseFromFile rawGamesParser filePath
    putStrLn (show result)