module Main where

import Test.Tasty                 (testGroup, defaultMain)
import Chesskell.PGNParserTest    (pgnParserTest)
import Chesskell.PreprocessorTest (preprocessorTest)
import Chesskell.ControllerTest   (controllerTest)

main :: IO ()
main = do
  pgnParserTest    <- pgnParserTest
  preprocessorTest <- preprocessorTest
  controllerTest   <- controllerTest
  defaultMain $ testGroup "" [pgnParserTest, preprocessorTest, controllerTest]