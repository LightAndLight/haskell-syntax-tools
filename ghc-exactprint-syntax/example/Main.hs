{-# LANGUAGE QuasiQuotes #-}

module Main where

import GHC.ExactPrint.AntiQuote (substExpr)
import GHC.ExactPrint.QQ (ParsedExpr, ParsedSource, hsExpr, hsModule)
import Language.Haskell.GHC.ExactPrint (exactPrint)

main :: IO ()
main = do
  putStrLn $
    exactPrint
      @ParsedSource
      [hsModule|
        module Main where

        x :: Int
        x = 1
      |]

  putStrLn $
    exactPrint
      @ParsedExpr
      [hsExpr|MyRecord{ x = 1, y = True }|]

  putStrLn $
    exactPrint
      @ParsedExpr
      [hsExpr|
      IndentedRecord
      { x = 1
      , y = True
      , z = _
      }
      |]

  putStrLn . exactPrint @ParsedExpr $
    substExpr
      [ ("placeholder_1", [hsExpr|a b|])
      , ("placeholder_2", [hsExpr|c + d|])
      ]
      [hsExpr|
      IndentedRecord
      { x = 1
      , y = placeholder_1
      , z = placeholder_2
      }
      |]
