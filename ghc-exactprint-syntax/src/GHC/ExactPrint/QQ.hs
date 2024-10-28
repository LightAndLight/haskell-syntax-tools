{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module GHC.ExactPrint.QQ (GHC.ParsedSource, hsModule, ParsedExpr, hsExpr) where

import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import GHC qualified
import GHC.Data.Bag (mapBag, unitBag)
import GHC.Data.FastString (mkFastString)
import GHC.Data.Strict qualified
import GHC.Driver.Config.Diagnostic (initDiagOpts, initPrintConfig)
import GHC.Driver.Errors (printMessages)
import GHC.Driver.Errors.Types (ErrorMessages, GhcMessage (..))
import GHC.ExactPrint.QQ.Orphans ()
import GHC.Paths (libdir)
import GHC.Types.Error
  ( DiagnosticMessage (..)
  , DiagnosticReason (..)
  , MsgEnvelope (..)
  , UnknownDiagnostic (..)
  , getMessages
  , mkMessages
  , mkSimpleDecorated
  )
import GHC.Types.SrcLoc
  ( SrcSpan (..)
  , mkRealSrcLoc
  , mkRealSrcSpan
  , realSrcLocSpan
  , realSrcSpanEnd
  , realSrcSpanStart
  , srcLocCol
  , srcLocFile
  , srcLocLine
  )
import GHC.Utils.Logger (getLogger)
import GHC.Utils.Outputable (text)
import Language.Haskell.GHC.ExactPrint (ExactPrint, makeDeltaAst)
import Language.Haskell.GHC.ExactPrint.Parsers
  ( Parser
  , ghcWrapper
  , initDynFlagsPure
  , parseExpr
  , parseModuleFromStringInternal
  )
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Exp, Lift (..), Loc (..), location)

reindent :: String -> Either (Int, Int, Int) (Int, Int, String)
reindent = go 0 0
  where
    go line col [] = Right (line, col, [])
    go !line !col (c : cs) =
      case c of
        '\n' -> go (line + 1) 0 cs
        ' ' -> go line (col + 1) cs
        _ ->
          (line,col,) . intercalate "\n"
            <$> case lines (c : cs) of
              [] -> pure []
              l : ls ->
                (l :)
                  <$> traverse
                    ( \(ix, val) ->
                        let (a, b) = splitAt col val
                        in if all (== ' ') a
                            then Right b
                            else Left (line, ix, length (takeWhile (== ' ') a) - 1)
                    )
                    (zip [1 ..] ls)

mkQuoteExp ::
  (ExactPrint a, Lift a) =>
  Parser a ->
  String ->
  Q Exp
mkQuoteExp parse input = do
  loc <- location
  let (quoteStartLine, _quoteStartCol) = loc_start loc
  result <- liftIO . ghcWrapper libdir $ do
    let path = loc_filename loc
    case reindent input of
      Left (l, lOffset, c) -> do
        dflags <- initDynFlagsPure path input
        logger <- getLogger
        namePprCtx <- GHC.getNamePprCtx
        let
          errs :: ErrorMessages
          errs =
            mkMessages . unitBag $
              MsgEnvelope
                { errMsgSpan =
                    RealSrcSpan
                      (realSrcLocSpan $ mkRealSrcLoc (mkFastString path) (quoteStartLine + l + lOffset) c)
                      GHC.Data.Strict.Nothing
                , errMsgContext = namePprCtx
                , errMsgDiagnostic =
                    GhcUnknownMessage $
                      UnknownDiagnostic
                        DiagnosticMessage
                          { diagMessage = mkSimpleDecorated $ text "Bad indentation in [hsModule|...|]"
                          , diagReason = ErrorWithoutFlag
                          , diagHints = []
                          }
                , errMsgSeverity = GHC.SevError
                }
        pure . Left $ do
          liftIO $ printMessages logger (initPrintConfig dflags) (initDiagOpts dflags) errs
          fail "Bad indentation"
      Right (l, c, input') -> do
        dflags <- initDynFlagsPure path input'
        let res = parse dflags path input'
        case res of
          Left errs -> do
            logger <- getLogger
            let
              errs' =
                mkMessages
                  $ mapBag
                    ( \msg ->
                        msg
                          { errMsgSpan =
                              case errMsgSpan msg of
                                UnhelpfulSpan reason -> UnhelpfulSpan reason
                                RealSrcSpan rss mb ->
                                  let
                                    addLine :: Int -> GHC.RealSrcLoc -> GHC.RealSrcLoc
                                    addLine n x =
                                      mkRealSrcLoc
                                        (srcLocFile x)
                                        (srcLocLine x + n)
                                        (srcLocCol x)

                                    addCol :: Int -> GHC.RealSrcLoc -> GHC.RealSrcLoc
                                    addCol n x =
                                      mkRealSrcLoc
                                        (srcLocFile x)
                                        (srcLocLine x)
                                        (srcLocCol x + n)
                                  in
                                    RealSrcSpan
                                      ( mkRealSrcSpan
                                          (addCol c . addLine (quoteStartLine + l - 1) $ realSrcSpanStart rss)
                                          (addCol c . addLine (quoteStartLine + l - 1) $ realSrcSpanEnd rss)
                                      )
                                      mb
                          }
                    )
                  $ getMessages errs
            pure . Left $ do
              liftIO $ printMessages logger (initPrintConfig dflags) (initDiagOpts dflags) errs'
              fail "Parse error"
          Right (makeDeltaAst -> parsed) ->
            pure $ Right parsed

  case result of
    Left printErrors ->
      printErrors
    Right ast ->
      lift ast

hsModule :: QuasiQuoter
hsModule =
  QuasiQuoter
    { quoteExp = mkQuoteExp parseModuleFromStringInternal
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec = undefined
    }

type ParsedExpr = GHC.LHsExpr GHC.GhcPs

hsExpr :: QuasiQuoter
hsExpr =
  QuasiQuoter
    { quoteExp = mkQuoteExp parseExpr
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec = undefined
    }
