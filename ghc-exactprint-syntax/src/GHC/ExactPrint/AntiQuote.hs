module GHC.ExactPrint.AntiQuote where

import GHC (GenLocated (..), GhcPs, HsExpr (..))
import GHC.ExactPrint.QQ (ParsedExpr)
import Generics.SYB (everywhere, mkT)
import Language.Haskell.GHC.ExactPrint.Utils (rdrName2String)

substExpr :: [(String, ParsedExpr)] -> ParsedExpr -> ParsedExpr
substExpr vars = everywhere (mkT f)
  where
    f :: HsExpr GhcPs -> HsExpr GhcPs
    f a@(HsVar _ (L _ ident)) =
      case lookup (rdrName2String ident) vars of
        Nothing -> a
        Just (L _ a') -> a'
    f a = a
