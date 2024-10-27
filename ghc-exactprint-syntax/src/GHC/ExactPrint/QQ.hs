{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module GHC.ExactPrint.QQ (GHC.ParsedSource, hsModule) where

import Control.Monad.IO.Class (liftIO)
import GHC qualified
import GHC.Driver.Config.Diagnostic (initDiagOpts, initPrintConfig)
import GHC.Driver.Errors (printMessages)
import GHC.Paths (libdir)
import GHC.Utils.Logger (getLogger)
import Language.Haskell.GHC.ExactPrint (makeDeltaAst)
import Language.Haskell.GHC.ExactPrint.Parsers
  ( ghcWrapper
  , initDynFlagsPure
  , parseModuleFromStringInternal
  )
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Lift (..), Loc (..), location)
import GHC.ExactPrint.QQ.Orphans ()

hsModule :: QuasiQuoter
hsModule =
  QuasiQuoter
    { quoteExp = \input -> do
        loc <- location
        result <- liftIO . ghcWrapper libdir $ do
          let path = loc_filename loc
          dflags <- initDynFlagsPure path input
          let res = parseModuleFromStringInternal dflags path input
          case res of
            Left errs -> do
              logger <- getLogger
              liftIO $ printMessages logger (initPrintConfig dflags) (initDiagOpts dflags) errs
              fail "xxx"
            Right (makeDeltaAst -> parsed) ->
              pure parsed
        lift result
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec = undefined
    }
