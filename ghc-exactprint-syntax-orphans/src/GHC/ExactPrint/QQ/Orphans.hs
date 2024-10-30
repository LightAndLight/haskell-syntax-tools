{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.ExactPrint.QQ.Orphans where

import GHC qualified
import GHC.Core.TyCo.Rep qualified
import GHC.Core.TyCon qualified
import GHC.Data.Bag qualified
import GHC.Data.BooleanFormula qualified
import GHC.Data.FastString (FastString, mkFastString, unpackFS)
import GHC.Data.Strict qualified
import GHC.Types.Basic qualified
import GHC.Types.Fixity qualified
import GHC.Types.ForeignCall qualified
import GHC.Types.Name qualified
import GHC.Types.Name.Occurrence qualified
import GHC.Types.Name.Reader (RdrName (..))
import GHC.Types.PkgQual qualified
import GHC.Types.SourceText qualified
import GHC.Types.SrcLoc
  ( BufPos (..)
  , BufSpan (..)
  , UnhelpfulSpanReason (..)
  , mkRealSrcLoc
  , mkRealSrcSpan
  , realSrcSpanEnd
  , realSrcSpanStart
  )
import GHC.Types.Unique qualified
import GHC.Types.Unique.DSet qualified
import GHC.Types.Var qualified
import GHC.Unit.Module.Warnings qualified
import GHC.Unit.Types qualified
import GHC.Utils.Outputable (Outputable, ppr, showSDocUnsafe)
import Language.Haskell.Syntax.Basic qualified
import Language.Haskell.TH.Syntax (Lift (..))

deriving instance (Lift l, Lift a) => Lift (GHC.GenLocated l a)
deriving instance Lift (GHC.HsModule GHC.GhcPs)
deriving instance Lift GHC.XModulePs
deriving instance Lift GHC.AnnsModule
deriving instance Lift (GHC.ImportDecl GHC.GhcPs)
deriving instance Lift GHC.XImportDeclPass
deriving instance Lift GHC.EpAnnImportDecl
deriving instance Lift GHC.Types.PkgQual.RawPkgQual
deriving instance Lift GHC.IsBootInterface
deriving instance Lift GHC.ImportDeclQualifiedStyle
deriving instance Lift GHC.ImportListInterpretation
deriving instance Lift (GHC.IE GHC.GhcPs)
deriving instance Lift GHC.IEWildcard
deriving instance Lift (GHC.IEWrappedName GHC.GhcPs)
deriving instance Lift (GHC.HsDecl GHC.GhcPs)
deriving instance Lift (GHC.HsBind GHC.GhcPs)
deriving instance Lift (GHC.PatSynBind GHC.GhcPs GHC.GhcPs)
deriving instance Lift (GHC.RecordPatSynField GHC.GhcPs)
deriving instance Lift (GHC.HsPatSynDir GHC.GhcPs)
deriving instance Lift (GHC.MatchGroup GHC.GhcPs (GHC.LocatedA (GHC.HsExpr GHC.GhcPs)))
deriving instance Lift GHC.Types.Basic.Origin
deriving instance Lift (GHC.Match GHC.GhcPs (GHC.LocatedA (GHC.HsExpr GHC.GhcPs)))
deriving instance Lift (GHC.HsMatchContext GHC.GhcPs)
deriving instance Lift GHC.HsArrowMatchContext
deriving instance Lift (GHC.HsStmtContext GHC.GhcPs)
deriving instance Lift (GHC.GRHSs GHC.GhcPs (GHC.LocatedA (GHC.HsExpr GHC.GhcPs)))

-- deriving instance (Lift a, Lift (GHC.Anno (GHC.GRHS GHC.GhcPs a))) => Lift (GHC.GRHSs GHC.GhcPs a)
deriving instance Lift a => Lift (GHC.GRHS GHC.GhcPs a)
deriving instance Lift GHC.GrhsAnn
deriving instance
  Lift (GHC.StmtLR GHC.GhcPs GHC.GhcPs (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsExpr GHC.GhcPs)))
deriving instance Lift GHC.TransForm
deriving instance Lift (GHC.ApplicativeArg GHC.GhcPs)
deriving instance Lift (GHC.ParStmtBlock GHC.GhcPs GHC.GhcPs)
deriving instance Lift (GHC.HsLocalBindsLR GHC.GhcPs GHC.GhcPs)
deriving instance Lift (GHC.HsIPBinds GHC.GhcPs)
deriving instance Lift (GHC.IPBind GHC.GhcPs)
instance Lift (GHC.HsValBindsLR GHC.GhcPs GHC.GhcPs) where
  liftTyped (GHC.ValBinds a b c) = [||GHC.ValBinds a b c||]
  liftTyped GHC.XValBindsLR{} = error "XValBindsLR"
deriving instance Lift (GHC.Sig GHC.GhcPs)
deriving instance Lift GHC.Types.Basic.InlinePragma
deriving instance Lift GHC.Types.Basic.RuleMatchInfo
deriving instance Lift GHC.Types.Basic.InlineSpec
deriving instance Lift GHC.AnnSig
deriving instance Lift a => Lift (GHC.Data.BooleanFormula.BooleanFormula a)
deriving instance Lift (GHC.HsSigType GHC.GhcPs)
deriving instance Lift (GHC.HsOuterSigTyVarBndrs GHC.GhcPs)
deriving instance Lift a => Lift (GHC.HsTyVarBndr a GHC.GhcPs)
deriving instance Lift (GHC.HsType GHC.GhcPs)
deriving instance Lift GHC.Types.Basic.PromotionFlag
deriving instance Lift GHC.HsSrcBang
deriving instance Lift Language.Haskell.Syntax.Basic.SrcUnpackedness
deriving instance Lift Language.Haskell.Syntax.Basic.SrcStrictness
deriving instance Lift GHC.HsTupleSort
deriving instance Lift GHC.AnnParen
deriving instance Lift GHC.ParenType
deriving instance Lift (GHC.HsTyLit GHC.GhcPs)
deriving instance Lift (GHC.HsUntypedSplice GHC.GhcPs)
deriving instance Lift (GHC.HsExpr GHC.GhcPs)
deriving instance Lift GHC.AnnProjection
deriving instance Lift GHC.HsDoFlavour
deriving instance Lift GHC.AnnExplicitSum
deriving instance Lift GHC.HsIPName
deriving instance Lift GHC.EpAnnUnboundVar
deriving instance Lift (GHC.HsPragE GHC.GhcPs)
deriving instance Lift (GHC.HsCmdTop GHC.GhcPs)
deriving instance Lift (GHC.HsCmd GHC.GhcPs)
deriving instance
  Lift (GHC.StmtLR GHC.GhcPs GHC.GhcPs (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsCmd GHC.GhcPs)))
deriving instance Lift (GHC.MatchGroup GHC.GhcPs (GHC.LocatedA (GHC.HsCmd GHC.GhcPs)))
deriving instance Lift (GHC.Match GHC.GhcPs (GHC.LocatedA (GHC.HsCmd GHC.GhcPs)))
deriving instance Lift (GHC.GRHSs GHC.GhcPs (GHC.LocatedA (GHC.HsCmd GHC.GhcPs)))
deriving instance Lift GHC.AnnsIf
deriving instance Lift GHC.LamCaseVariant
deriving instance Lift GHC.EpAnnHsCase
deriving instance Lift GHC.HsArrAppType
deriving instance Lift (GHC.HsQuote GHC.GhcPs)
deriving instance Lift (GHC.HsGroup GHC.GhcPs)
deriving instance Lift (GHC.WarnDecls GHC.GhcPs)
deriving instance Lift (GHC.WarnDecl GHC.GhcPs)
deriving instance Lift (GHC.Unit.Module.Warnings.WarningTxt GHC.GhcPs)
deriving instance Lift GHC.Types.SourceText.StringLiteral
deriving instance Lift (GHC.ForeignDecl GHC.GhcPs)
deriving instance Lift (GHC.ForeignExport GHC.GhcPs)
deriving instance Lift GHC.Types.ForeignCall.CExportSpec
deriving instance Lift (GHC.ForeignImport GHC.GhcPs)
deriving instance Lift GHC.Types.ForeignCall.Safety
deriving instance Lift GHC.Types.ForeignCall.CCallConv
deriving instance Lift GHC.CImportSpec
deriving instance Lift GHC.Types.ForeignCall.CCallTarget
deriving instance Lift (GHC.DefaultDecl GHC.GhcPs)
deriving instance Lift (GHC.FixitySig GHC.GhcPs)
deriving instance Lift GHC.Types.Fixity.Fixity
deriving instance Lift GHC.Types.Fixity.FixityDirection
deriving instance Lift (GHC.DerivDecl GHC.GhcPs)
deriving instance Lift (GHC.TyClGroup GHC.GhcPs)
deriving instance Lift (GHC.TyClDecl GHC.GhcPs)
deriving instance Lift (GHC.LayoutInfo GHC.GhcPs)
deriving instance Lift (GHC.FunDep GHC.GhcPs)
deriving instance Lift (GHC.FamilyDecl GHC.GhcPs)
deriving instance Lift GHC.Types.Basic.TopLevelFlag
deriving instance Lift (GHC.LHsQTyVars GHC.GhcPs)
deriving instance Lift (GHC.FamilyResultSig GHC.GhcPs)
deriving instance Lift (GHC.InjectivityAnn GHC.GhcPs)
deriving instance Lift (GHC.FamilyInfo GHC.GhcPs)
deriving instance Lift (GHC.RoleAnnotDecl GHC.GhcPs)
deriving instance Lift Language.Haskell.Syntax.Basic.Role
deriving instance Lift (GHC.StandaloneKindSig GHC.GhcPs)
deriving instance Lift (GHC.InstDecl GHC.GhcPs)
deriving instance Lift (GHC.ClsInstDecl GHC.GhcPs)
deriving instance Lift GHC.Types.Basic.OverlapMode
deriving instance Lift GHC.AnnSortKey
deriving instance Lift (GHC.TyFamInstDecl GHC.GhcPs)
deriving instance Lift (GHC.DataFamInstDecl GHC.GhcPs)
deriving instance Lift (GHC.HsDataDefn GHC.GhcPs)
deriving instance Lift GHC.Types.ForeignCall.CType
deriving instance Lift GHC.Types.ForeignCall.Header
deriving instance Lift (GHC.HsDerivingClause GHC.GhcPs)
deriving instance Lift (GHC.DerivStrategy GHC.GhcPs)
deriving instance Lift GHC.XViaStrategyPs
deriving instance Lift (GHC.DerivClauseTys GHC.GhcPs)
deriving instance Lift (GHC.ConDecl GHC.GhcPs)
deriving instance Lift GHC.AnnContext
deriving instance Lift GHC.IsUnicodeSyntax
deriving instance Lift (GHC.HsConDeclGADTDetails GHC.GhcPs)
deriving instance Lift (GHC.ConDeclField GHC.GhcPs)
deriving instance Lift a => Lift (GHC.HsScaled GHC.GhcPs a)
deriving instance Lift a => Lift (GHC.DataDefnCons a)
deriving instance Lift a => Lift (GHC.FamEqn GHC.GhcPs a)
deriving instance Lift GHC.LexicalFixity
deriving instance Lift (GHC.HsOuterFamEqnTyVarBndrs GHC.GhcPs)
deriving instance (Lift a, Lift b) => Lift (GHC.HsArg a b)
deriving instance Lift (GHC.SpliceDecl GHC.GhcPs)
deriving instance Lift GHC.SpliceDecoration
deriving instance Lift (GHC.AnnDecl GHC.GhcPs)
deriving instance Lift GHC.AnnPragma
deriving instance Lift (GHC.AnnProvenance GHC.GhcPs)
deriving instance Lift (GHC.RuleDecls GHC.GhcPs)
deriving instance Lift (GHC.RuleDecl GHC.GhcPs)
deriving instance Lift GHC.Types.Basic.Activation
deriving instance Lift GHC.HsRuleAnn
deriving instance Lift (GHC.RuleBndr GHC.GhcPs)
deriving instance Lift (GHC.DocDecl GHC.GhcPs)
deriving instance Lift (GHC.Pat GHC.GhcPs)
deriving instance Lift GHC.EpAnnSumPat
deriving instance Lift Language.Haskell.Syntax.Basic.Boxity
deriving instance Lift GHC.AnnList
deriving instance Lift (GHC.HsConPatTyArg GHC.GhcPs)
deriving instance Lift (GHC.HsPatSigType GHC.GhcPs)
deriving instance (Lift a, Lift b, Lift c) => Lift (GHC.HsConDetails a b c)
deriving instance Lift (GHC.ArithSeqInfo GHC.GhcPs)
deriving instance Lift (GHC.FieldLabelStrings GHC.GhcPs)
deriving instance Lift (GHC.DotFieldOcc GHC.GhcPs)
deriving instance Lift GHC.AnnFieldLabel
deriving instance Lift Language.Haskell.Syntax.Basic.FieldLabelString
deriving instance Lift (GHC.AmbiguousFieldOcc GHC.GhcPs)
deriving instance Lift (GHC.HsTupArg GHC.GhcPs)
deriving instance Lift (GHC.HsLit GHC.GhcPs)
instance Lift GHC.Core.TyCo.Rep.Type where
  liftTyped _ = error "lift called for GHC.Core.TyCo.Rep.Type"
deriving instance Lift (GHC.HsOverLit GHC.GhcPs)
deriving instance Lift GHC.OverLitVal
deriving instance Lift GHC.Types.SourceText.FractionalLit
deriving instance Lift GHC.Types.SourceText.FractionalExponentBase
deriving instance Lift GHC.Types.SourceText.IntegralLit
deriving instance Lift GHC.Types.SourceText.SourceText
deriving instance Lift a => Lift (GHC.HsRecFields GHC.GhcPs a)
deriving instance Lift GHC.RecFieldsDotDot
deriving instance Lift GHC.NoEpAnns
deriving instance Lift (GHC.FieldOcc GHC.GhcPs)
deriving instance Lift GHC.NameAnn
deriving instance Lift GHC.NameAdornment
deriving instance (Lift a, Lift b) => Lift (GHC.HsFieldBind a b)
deriving instance Lift (GHC.HsArrow GHC.GhcPs)
deriving instance Lift (GHC.HsLinearArrowTokens GHC.GhcPs)
deriving instance Lift GHC.TokenLocation
deriving instance Lift (GHC.HsToken a)
deriving instance Lift (GHC.HsUniToken a b)
deriving instance Lift (GHC.HsForAllTelescope GHC.GhcPs)
deriving instance Lift GHC.Types.Var.Specificity
deriving instance Lift GHC.AnnListItem
deriving instance Lift GHC.TrailingAnn
deriving instance Lift GHC.AddEpAnn
deriving instance Lift GHC.EpaLocation
deriving instance Lift GHC.AnnKeywordId
deriving instance Lift a => Lift (GHC.WithHsDocIdentifiers a GHC.GhcPs)
deriving instance Lift RdrName
instance Lift GHC.Name where
  liftTyped x
    | GHC.Types.Name.isExternalName x =
        let
          a = GHC.Types.Name.nameUnique x
          b = GHC.Types.Name.nameModule x
          c = GHC.Types.Name.nameOccName x
          d = GHC.Types.Name.nameSrcSpan x
        in
          [||GHC.Types.Name.mkExternalName a b c d||]
    | otherwise = error $ "lift for Name: " <> showSDocUnsafe (ppr x) <> " " <> classify
    where
      classify
        | GHC.Types.Name.isSystemName x = "system"
        | GHC.Types.Name.isInternalName x = "internal"
        | GHC.Types.Name.isExternalName x = "external"
        | GHC.Types.Name.isTyVarName x = "tyVar"
        | GHC.Types.Name.isTyConName x = "tyCon"
        | GHC.Types.Name.isDataConName x = "dataCon"
        | GHC.Types.Name.isValName x = "val"
        | GHC.Types.Name.isVarName x = "var"
        | otherwise = "unknown"
instance Lift GHC.Types.Name.Occurrence.OccName where
  liftTyped x =
    let
      a = GHC.Types.Name.Occurrence.occNameSpace x
      b = GHC.Types.Name.Occurrence.occNameString x
    in
      [||GHC.Types.Name.Occurrence.mkOccName a b||]
instance Lift GHC.Types.Name.Occurrence.NameSpace where
  liftTyped x
    | GHC.Types.Name.Occurrence.isVarNameSpace x = [||GHC.Types.Name.Occurrence.varName||]
    | GHC.Types.Name.Occurrence.isDataConNameSpace x = [||GHC.Types.Name.Occurrence.dataName||]
    | GHC.Types.Name.Occurrence.isTvNameSpace x = [||GHC.Types.Name.Occurrence.tvName||]
    | GHC.Types.Name.Occurrence.isTcClsNameSpace x = [||GHC.Types.Name.Occurrence.tcClsName||]
    | otherwise =
        error $ "unknown NameSpace: " <> showSDocUnsafe (GHC.Types.Name.Occurrence.pprNameSpace x)
deriving instance Lift GHC.Unit.Types.UnitId
deriving instance Lift a => Lift (GHC.Unit.Types.Definite a)
deriving instance Lift a => Lift (GHC.Unit.Types.GenInstantiatedUnit a)
instance Lift GHC.Types.Unique.Unique where
  liftTyped x =
    let a = GHC.Types.Unique.getKey x
    in [||GHC.Types.Unique.mkUniqueGrimily a||]
deriving instance Lift a => Lift (GHC.Unit.Types.GenUnit a)
deriving instance Lift a => Lift (GHC.Unit.Types.GenModule a)
deriving instance Lift GHC.ModuleName
instance Lift (GHC.Types.Unique.DSet.UniqDSet GHC.ModuleName) where
  liftTyped x = error $ "lift for UniqDSet ModuleName: " <> showSDocUnsafe (ppr x) -- [|| GHC.Types.Unique.DSet.mkUniqDSet (GHC.Types.Unique.DSet.uniqDSetToList x) ||]
deriving instance Lift a => Lift (GHC.HsWildCardBndrs GHC.GhcPs a)
deriving instance Lift GHC.DataConCantHappen
deriving instance Lift GHC.NoExtField
instance (Lift a, Outputable a) => Lift (GHC.Data.Bag.Bag a) where
  liftTyped x = error $ "lift for Bag: " <> showSDocUnsafe (ppr x) -- [|| GHC.Data.Bag.listToBag (GHC.Data.Bag.bagToList x) ||]
deriving instance Lift a => Lift (GHC.EpAnn a)
deriving instance Lift GHC.EpAnnComments
deriving instance Lift GHC.EpaComment
deriving instance Lift GHC.EpaCommentTok
deriving instance Lift GHC.HsDocString
deriving instance Lift GHC.HsDocStringDecorator
deriving instance Lift GHC.HsDocStringChunk
deriving instance Lift GHC.Anchor
deriving instance Lift GHC.AnchorOperation
deriving instance Lift GHC.DeltaPos
deriving instance Lift a => Lift (GHC.SrcSpanAnn' a)
deriving instance Lift GHC.SrcSpan
deriving instance Lift UnhelpfulSpanReason
instance Lift FastString where
  liftTyped x = let x' = unpackFS x in [||mkFastString x'||]
deriving instance Lift BufSpan
deriving instance Lift BufPos
instance Lift GHC.RealSrcSpan where
  liftTyped x =
    let start = realSrcSpanStart x
    in let end = realSrcSpanEnd x
       in [||mkRealSrcSpan start end||]
instance Lift GHC.RealSrcLoc where
  liftTyped x =
    let a = GHC.srcLocFile x
    in let b = GHC.srcLocLine x
       in let c = GHC.srcLocCol x
          in [||mkRealSrcLoc a b c||]
deriving instance Lift a => Lift (GHC.Data.Strict.Maybe a)
