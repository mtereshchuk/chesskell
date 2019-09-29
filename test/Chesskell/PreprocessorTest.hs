module Chesskell.PreprocessorTest
  ( preprocessorTest
  ) where

import Test.Tasty             (TestTree)
import Test.Tasty.Hspec       (Spec, testSpec, describe, it, shouldSatisfy, shouldBe)
import Chesskell.Preprocessor (preprocess)

shouldBeByPaths :: FilePath -> FilePath -> IO ()
shouldBeByPaths rawGamesPath gamesPath = do
  rawGamesString    <- readFile rawGamesPath
  let rawGames      = read rawGamesString
  gamesString       <- readFile gamesPath
  let games         = read gamesString
  preprocess rawGames `shouldBe` Right games

preprocessorSpec :: Spec
preprocessorSpec =
  describe "Preprocess success" $ do
    it "Simple" $ 
      shouldBeByPaths sucSimpleRawGames sucSimpleGames
    it "Hard" $ 
      shouldBeByPaths sucHardRawGames sucHardGames
  where
    prsPathPrefix        = "test-resources/prs/"
    pprcsPathPrefix      = "test-resources/pprcs/"
    prsFileExtension     = ".prs"
    pprcsFileExtension   = ".pprcs"
    toPrsPath fileName   = prsPathPrefix ++ fileName ++ prsFileExtension
    toPprcsPath fileName = pprcsPathPrefix ++ fileName ++ pprcsFileExtension
    sucSimpleRawGames    = toPrsPath "Success_Simple"
    sucSimpleGames       = toPprcsPath "Success_Simple"
    sucHardRawGames      = toPrsPath "Success_Hard"
    sucHardGames         = toPprcsPath "Success_Hard"
    
preprocessorTest :: IO TestTree
preprocessorTest = testSpec "Preprocessor" preprocessorSpec