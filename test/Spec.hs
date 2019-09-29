module Main where

import Test.Tasty                 (testGroup, defaultMain)
import Chesskell.PGNParserTest    (pgnParserTest)
import Chesskell.PreprocessorTest (preprocessorTest)
import Chesskell.ControlTest      (controlTest)

main :: IO ()
main = do
  pgnParserTest    <- pgnParserTest
  preprocessorTest <- preprocessorTest
  controlTest      <- controlTest
  defaultMain $ testGroup "" [pgnParserTest, preprocessorTest, controlTest]