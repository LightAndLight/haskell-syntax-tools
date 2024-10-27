{-# LANGUAGE QuasiQuotes #-}

module Main where

import GHC.ExactPrint.QQ (ParsedSource, hsModule)
import Language.Haskell.GHC.ExactPrint (exactPrint)

main :: IO ()
main =
  putStrLn $
    exactPrint
      @ParsedSource
      [hsModule|
        module Main where

        x :: Int
        x = 1
      |]
