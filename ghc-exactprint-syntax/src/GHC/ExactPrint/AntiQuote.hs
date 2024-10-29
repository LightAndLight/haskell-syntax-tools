module GHC.ExactPrint.AntiQuote where

import Data.Data (Data)
import Data.Maybe (fromMaybe)
import GHC
  ( Anchor (..)
  , AnchorOperation (..)
  , AnnListItem (..)
  , EpAnn (..)
  , EpaLocation (..)
  , ExprLStmt
  , GenLocated (..)
  , GhcPs
  , HsExpr (..)
  , HsToken (..)
  , LHsExpr
  , ModuleName
  , NoExtField (..)
  , SrcSpanAnn' (SrcSpanAnn)
  , StmtLR (..)
  , TokenLocation (..)
  , deltaPos
  , emptyComments
  , moduleNameString
  , realSrcSpan
  )
import GHC.Types.SrcLoc (generatedSrcSpan)
import Generics.SYB (everywhere, mkT)
import Language.Haskell.GHC.ExactPrint.Utils (rdrName2String)

substExpr :: Data a => [(String, HsExpr GhcPs)] -> a -> a
substExpr vars = everywhere (mkT go)
  where
    go :: HsExpr GhcPs -> HsExpr GhcPs
    go a@(HsVar _ (L _ ident)) =
      fromMaybe a $ lookup (rdrName2String ident) vars
    go (HsApp a b (L c (HsApp d e f))) =
      HsApp
        a
        b
        ( L
            c
            ( HsPar
                d
                (L (TokenLoc $ EpaDelta (deltaPos 0 0) []) HsTok)
                (L (SrcSpanAnn EpAnnNotUsed generatedSrcSpan) (HsApp EpAnnNotUsed e f))
                (L (TokenLoc $ EpaDelta (deltaPos 0 0) []) HsTok)
            )
        )
    go a = a

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

    movedAnchor :: Int -> Int -> Anchor
    movedAnchor l c = Anchor (realSrcSpan generatedSrcSpan) (MovedAnchor $ deltaPos l c)

    g :: ExprLStmt GhcPs -> [ExprLStmt GhcPs]
    g (L loc (BodyStmt NoExtField (L _ (HsVar _ (L _ ident))) NoExtField NoExtField))
      | Just stmts <- lookup (rdrName2String ident) vars =
          let addLoc = L (SrcSpanAnn (EpAnn (movedAnchor 1 0) (AnnListItem []) emptyComments) generatedSrcSpan)
          in case stmts of
              [] -> []
              [stmt] -> [L loc stmt]
              stmt : stmts' -> L loc stmt : fmap addLoc stmts'
    g a = [a]
