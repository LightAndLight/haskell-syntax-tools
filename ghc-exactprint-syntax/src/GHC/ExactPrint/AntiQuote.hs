module GHC.ExactPrint.AntiQuote where

import Data.Data (Data)
import Data.Maybe (fromMaybe)
import GHC
  ( ExprLStmt
  , GenLocated (..)
  , GhcPs
  , HsExpr (..)
  , LHsExpr
  , ModuleName
  , NoExtField (..)
  , StmtLR (..)
  , moduleNameString
  )
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

substDoStmts :: Data a => [(String, [StmtLR GhcPs GhcPs (LHsExpr GhcPs)])] -> a -> a
substDoStmts vars = everywhere (mkT f)
  where
    f :: HsExpr GhcPs -> HsExpr GhcPs
    f (HsDo ext flavour (L loc stmts)) = HsDo ext flavour . L loc $ g =<< stmts
    f a = a

    g :: ExprLStmt GhcPs -> [ExprLStmt GhcPs]
    g (L loc (BodyStmt NoExtField (L _ (HsVar _ (L _ ident))) NoExtField NoExtField))
      | Just stmts <- lookup (rdrName2String ident) vars = L loc <$> stmts
    g a = [a]
