module Main where

import Test.Tasty               (testGroup, defaultMain)
import Chesskell.ControllerTest (controllerTest)

main :: IO ()
main = do
  controllerTest <- controllerTest
  defaultMain $ testGroup "" [controllerTest]
