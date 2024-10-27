{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Applicative ((<**>))
import Control.Monad.IO.Class (liftIO)
import GHC.Driver.Config.Diagnostic (initDiagOpts, initPrintConfig)
import GHC.Driver.Errors (printMessages)
import GHC.ExactPrint.Debug
import GHC.Paths (libdir)
import GHC.Utils.Logger (getLogger)
import GHC.Utils.Outputable (showSDocUnsafe)
import Language.Haskell.GHC.ExactPrint (makeDeltaAst)
import Language.Haskell.GHC.ExactPrint.Parsers
  ( defaultCppOptions
  , ghcWrapper
  , initDynFlags
  , parseModuleEpAnnsWithCppInternal
  , postParseTransform
  )
import Options.Applicative qualified as Options

newtype Cli = Cli FilePath

cliParser :: Options.Parser Cli
cliParser =
  Cli
    <$> Options.strArgument
      (Options.metavar "FILE" <> Options.help "Dump the exact-print syntax tree for FILE")

main :: IO ()
main = do
  Cli file <- Options.execParser (Options.info (cliParser <**> Options.helper) Options.fullDesc)
  ghcWrapper libdir $ do
    dflags <- initDynFlags file
    res <- parseModuleEpAnnsWithCppInternal defaultCppOptions dflags file
    case postParseTransform res of
      Left errs -> do
        logger <- getLogger
        liftIO $ printMessages logger (initPrintConfig dflags) (initDiagOpts dflags) errs
      Right (makeDeltaAst -> parsed) ->
        liftIO . putStrLn . showSDocUnsafe $
          debugGenLocated debugSrcSpan debugHsModule parsed
