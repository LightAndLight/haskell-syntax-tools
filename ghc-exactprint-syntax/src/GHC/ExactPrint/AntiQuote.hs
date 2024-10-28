module GHC.ExactPrint.AntiQuote where

import Data.Data (Data)
import Data.Maybe (fromMaybe)
import GHC (GenLocated (..), GhcPs, HsExpr (..), ModuleName, moduleNameString)
import Generics.SYB (everywhere, mkT)
import Language.Haskell.GHC.ExactPrint.Utils (rdrName2String)

substExpr :: Data a => [(String, HsExpr GhcPs)] -> a -> a
substExpr vars = everywhere (mkT f)
  where
    f :: HsExpr GhcPs -> HsExpr GhcPs
    f a@(HsVar _ (L _ ident)) = fromMaybe a $ lookup (rdrName2String ident) vars
    f a = a

substModuleName :: Data a => [(String, ModuleName)] -> a -> a
substModuleName vars = everywhere (mkT f)
  where
    f :: ModuleName -> ModuleName
    f a = fromMaybe a $ lookup (moduleNameString a) vars
