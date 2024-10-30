{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Data (Data)
import Data.Maybe (fromMaybe)
import GHC qualified
import GHC.ExactPrint.QQ (ParsedSource, hsExpr, hsModule)
import GHC.ExactPrint.Transform (addItemToExplicitList)
import Generics.SYB (everywhere, geq, gshow, mkT)
import Language.Haskell.GHC.ExactPrint (exactPrint)
import Test.Hspec

data Test = Test
  { desc :: String
  , input :: ParsedSource
  , f :: ParsedSource -> ParsedSource
  , expected :: ParsedSource
  }

test :: Test -> SpecWith ()
test t =
  it t.desc $ do
    let actual = t.f t.input
    exactPrint actual `shouldBe` exactPrint t.expected

main :: IO ()
main =
  hspec $ do
    describe "addItemToExplicitList" $ do
      let
        f =
          everywhere
            (mkT (\expr -> fromMaybe expr $ addItemToExplicitList (GHC.unLoc [hsExpr|x|]) expr))

      test
        Test
          { desc = "add to empty single-line list"
          , input =
              [hsModule|
              module A where

              l = []
              |]
          , f = f
          , expected =
              [hsModule|
              module A where

              l = [x]
              |]
          }

      test
        Test
          { desc = "add to empty single-line list containing a space"
          , input =
              [hsModule|
              module A where

              l = [ ]
              |]
          , f = f
          , expected =
              [hsModule|
              module A where

              l = [x ]
              |]
          }

      test
        Test
          { desc = "add to single-item multi-line list"
          , input =
              [hsModule|
              module A where

              l =
                [ a
                ]
              |]
          , f = f
          , expected =
              [hsModule|
              module A where

              l =
                [ a
                , x
                ]
              |]
          }

      test
        Test
          { desc = "add to multi-item multi-line list"
          , input =
              [hsModule|
              module A where

              l =
                [ a
                , b
                ]
              |]
          , f = f
          , expected =
              [hsModule|
              module A where

              l =
                [ a
                , b
                , x
                ]
              |]
          }
