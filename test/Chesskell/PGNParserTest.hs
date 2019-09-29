module Chesskell.PGNParserTest
  ( pgnParserTest
  ) where

import Data.Either         (isLeft)
import Test.Tasty          (TestTree)
import Test.Tasty.Hspec    (Spec, testSpec, describe, it, shouldSatisfy, shouldBe)
import Chesskell.Chess     (Color (..), PieceType (..))
import Chesskell.PGNParser (RawTag (..), RawMove (..), RawGame (..), X (..), Y (..), parsePGNFile)

pgnParserSpec :: Spec
pgnParserSpec = do
  describe "Parse tags fails" $ do
    it "Tag close missing" $ do
      parseRes <- parsePGNFile tagCloseMissing
      fail parseRes
    it "Text between tags" $ do
      parseRes <- parsePGNFile textBetweenTags
      fail parseRes
  describe "Parse moves fails" $ do
    it "Unknow piece type" $ do
      parseRes <- parsePGNFile unknownPieceType
      fail parseRes
    it "Unexpected letter" $ do
      parseRes <- parsePGNFile unexpectedLetter
      fail parseRes
    it "Unexpected digit" $ do
      parseRes <- parsePGNFile unexpectedDigit
      fail parseRes
  describe "Total parse success" $ do
    it "Simple" $ do
      parseRes <- parsePGNFile simple
      parseRes `shouldBe` simpleExpected
    it "All possible cases" $ do
      parseRes <- parsePGNFile allPossibleCases
      parseRes `shouldBe` allPosExpected
  where
    fail             = flip shouldSatisfy isLeft
    pathPrefix       = "test-resources/pgn/"
    fileExtension    = ".pgn"
    toPath fileName  = pathPrefix ++ fileName ++ fileExtension
    tagCloseMissing  = toPath "Tag_Close_Missing"
    textBetweenTags  = toPath "Text_Between_Tags"
    unknownPieceType = toPath "Unknown_Piece_Type"
    unexpectedLetter = toPath "Unexpected_Letter"
    unexpectedDigit  = toPath "Unexpected_Digit"
    simple           = toPath "Simple"
    allPossibleCases = toPath "All_Possible_Cases"
    simpleExpected   = Right
      [ RawGame
        { rawTags   =
          [ RawTag
            { rawTagName  = "Event"
            , rawTagValue = "Test"
            }
          ]
        , rawMoves  =
          [ BaseRawMove
            { pieceType     = Rook
            , rawExtraCoord = Nothing
            , wasCapture    = False
            , rawPosition   = (X 'a', Y 1)
            , turnIntoType  = Nothing
            , wasCheck      = False
            , wasMate       = False
            }
          ]
        , rawWinner = Just Black
        }
      ]
    allPosExpected   = Right
      [ RawGame
        { rawTags   =
          [ RawTag
            { rawTagName  = "Event"
            , rawTagValue = "Test1"
            }
          ]
        , rawMoves  =
          [ BaseRawMove
            { pieceType     = King
            , rawExtraCoord = Nothing
            , wasCapture    = True
            , rawPosition   = (X 'a', Y 1)
            , turnIntoType  = Nothing
            , wasCheck      = False
            , wasMate       = False
            }
          , BaseRawMove
            { pieceType     = Queen
            , rawExtraCoord = Nothing
            , wasCapture    = False
            , rawPosition   = (X 'b', Y 2)
            , turnIntoType  = Nothing
            , wasCheck      = True
            , wasMate       = False
            }
            , BaseRawMove
            { pieceType     = Bishop
            , rawExtraCoord = Just $ Left $ X 'a'
            , wasCapture    = False
            , rawPosition   = (X 'c', Y 3)
            , turnIntoType  = Nothing
            , wasCheck      = False
            , wasMate       = True
            }
            , BaseRawMove
            { pieceType     = Knight
            , rawExtraCoord = Just $ Right $ Y 1
            , wasCapture    = False
            , rawPosition   = (X 'd', Y 4)
            , turnIntoType  = Nothing
            , wasCheck      = False
            , wasMate       = False
            }
            , BaseRawMove
            { pieceType     = Rook
            , rawExtraCoord = Nothing
            , wasCapture    = False
            , rawPosition   = (X 'e', Y 5)
            , turnIntoType  = Nothing
            , wasCheck      = False
            , wasMate       = False
            }
            , BaseRawMove
            { pieceType     = Pawn
            , rawExtraCoord = Nothing
            , wasCapture    = False
            , rawPosition   = (X 'f', Y 6)
            , turnIntoType  = Just Queen
            , wasCheck      = False
            , wasMate       = False
            }
            , ShortCastling
            { wasCheck = True
            , wasMate  = False
            }
            , LongCastling
            { wasCheck = False
            , wasMate  = True
            }
          ]
        , rawWinner = Just White
        }
      , RawGame
        { rawTags   =
          [ RawTag
            { rawTagName  = "Event"
            , rawTagValue = "Test2"
            }
          ]
        , rawMoves  =
          [ BaseRawMove
            { pieceType     = Rook
            , rawExtraCoord = Nothing
            , wasCapture    = False
            , rawPosition   = (X 'a', Y 1)
            , turnIntoType  = Nothing
            , wasCheck      = False
            , wasMate       = False
            }
          ]
        , rawWinner = Nothing
        }
      , RawGame
        { rawTags   =
          [ RawTag
            { rawTagName  = "Event"
            , rawTagValue = "Test3"
            }
          ]
        , rawMoves  =
          [ BaseRawMove
            { pieceType     = Rook
            , rawExtraCoord = Nothing
            , wasCapture    = False
            , rawPosition   = (X 'a', Y 1)
            , turnIntoType  = Nothing
            , wasCheck      = False
            , wasMate       = False
            }
          ]
        , rawWinner = Just Black
        }
      ]
      
pgnParserTest :: IO TestTree
pgnParserTest = testSpec "PGNParser" pgnParserSpec