module Main where

import Test.Tasty               (testGroup, defaultMain)
import Chesskell.PGNParserTest  (pgnParserTest)
import Chesskell.ControllerTest (controllerTest)

main :: IO ()
main = do
  pgnParserTest  <- pgnParserTest
  controllerTest <- controllerTest
  defaultMain $ testGroup "" [pgnParserTest, controllerTest]