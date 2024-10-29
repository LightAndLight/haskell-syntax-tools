module GHC.ExactPrint.Syntax where

import Control.Lens.At (ix)
import Control.Lens.Setter ((.~))
import Data.Function ((&))
import Data.Functor ((<&>))
import GHC (GenLocated (..), GhcPs)
import GHC qualified
import GHC.Lens
import GHC.Types.Name.Occurrence (mkVarOcc)
import GHC.Types.Name.Reader (mkRdrUnqual)
import GHC.Types.PkgQual (RawPkgQual (..))
import GHC.Types.SourceText (SourceText (..))
import GHC.Types.SrcLoc (generatedSrcSpan)

generatedSrcSpanAnn :: GHC.SrcSpanAnn' (GHC.EpAnn ann)
generatedSrcSpanAnn = GHC.SrcSpanAnn GHC.EpAnnNotUsed generatedSrcSpan

movedAnchor :: Int -> Int -> GHC.Anchor
movedAnchor l c =
  GHC.Anchor (GHC.realSrcSpan generatedSrcSpan) (GHC.MovedAnchor $ GHC.deltaPos l c)

epaDelta :: Int -> Int -> GHC.EpaLocation
epaDelta l c = GHC.EpaDelta (GHC.deltaPos l c) []

{-| Simple imports

* @import ModuleName (items)@
* @import ModuleName@
-}
import_ ::
  -- | Module name
  String ->
  -- | Import items
  Maybe [GHC.IE GhcPs] ->
  GHC.ImportDecl GhcPs
import_ moduleName mItems =
  GHC.ImportDecl
    { GHC.ideclExt =
        GHC.XImportDeclPass
          { GHC.ideclAnn =
              GHC.EpAnn
                (movedAnchor 0 0)
                GHC.EpAnnImportDecl
                  { GHC.importDeclAnnImport = epaDelta 0 0
                  , GHC.importDeclAnnPragma = Nothing
                  , GHC.importDeclAnnSafe = Nothing
                  , GHC.importDeclAnnQualified = Nothing
                  , GHC.importDeclAnnPackage = Nothing
                  , GHC.importDeclAnnAs = Nothing
                  }
                GHC.emptyComments
          , GHC.ideclSourceText = NoSourceText
          , GHC.ideclImplicit = False
          }
    , GHC.ideclName =
        L
          ( GHC.SrcSpanAnn
              ( GHC.EpAnn
                  (movedAnchor 0 1)
                  (GHC.AnnListItem [])
                  GHC.emptyComments
              )
              generatedSrcSpan
          )
          (GHC.mkModuleName moduleName)
    , GHC.ideclPkgQual = NoRawPkgQual
    , GHC.ideclSource = GHC.NotBoot
    , GHC.ideclSafe = False
    , GHC.ideclQualified = GHC.NotQualified
    , GHC.ideclAs = Nothing
    , GHC.ideclImportList =
        mItems <&> \items ->
          ( GHC.Exactly
          , L
              ( GHC.SrcSpanAnn
                  ( GHC.EpAnn
                      (movedAnchor 0 1)
                      GHC.AnnList
                        { GHC.al_anchor = Nothing
                        , GHC.al_open = Just $ GHC.AddEpAnn GHC.AnnOpenP (epaDelta 0 0)
                        , GHC.al_close = Just $ GHC.AddEpAnn GHC.AnnCloseP (epaDelta 0 0)
                        , GHC.al_rest = []
                        , GHC.al_trailing = []
                        }
                      GHC.emptyComments
                  )
                  generatedSrcSpan
              )
              (commaSeparate items)
          )
    }

{-| Simple qualified imports

* @import qualified ModuleName (items)@
* @import qualified ModuleName@
-}
importQualified ::
  -- | Module name
  String ->
  -- | Import items
  Maybe [GHC.IE GhcPs] ->
  GHC.ImportDecl GhcPs
importQualified moduleName mItems =
  GHC.ImportDecl
    { GHC.ideclExt =
        GHC.XImportDeclPass
          { GHC.ideclAnn =
              GHC.EpAnn
                (movedAnchor 0 0)
                GHC.EpAnnImportDecl
                  { GHC.importDeclAnnImport = epaDelta 0 0
                  , GHC.importDeclAnnPragma = Nothing
                  , GHC.importDeclAnnSafe = Nothing
                  , GHC.importDeclAnnQualified = Just $ epaDelta 0 1
                  , GHC.importDeclAnnPackage = Nothing
                  , GHC.importDeclAnnAs = Nothing
                  }
                GHC.emptyComments
          , GHC.ideclSourceText = NoSourceText
          , GHC.ideclImplicit = False
          }
    , GHC.ideclName =
        L
          ( GHC.SrcSpanAnn
              ( GHC.EpAnn
                  (movedAnchor 0 1)
                  (GHC.AnnListItem [])
                  GHC.emptyComments
              )
              generatedSrcSpan
          )
          (GHC.mkModuleName moduleName)
    , GHC.ideclPkgQual = NoRawPkgQual
    , GHC.ideclSource = GHC.NotBoot
    , GHC.ideclSafe = False
    , GHC.ideclQualified = GHC.QualifiedPre
    , GHC.ideclAs = Nothing
    , GHC.ideclImportList =
        mItems <&> \items ->
          ( GHC.Exactly
          , L
              ( GHC.SrcSpanAnn
                  ( GHC.EpAnn
                      (movedAnchor 0 1)
                      GHC.AnnList
                        { GHC.al_anchor = Nothing
                        , GHC.al_open = Just $ GHC.AddEpAnn GHC.AnnOpenP (epaDelta 0 0)
                        , GHC.al_close = Just $ GHC.AddEpAnn GHC.AnnCloseP (epaDelta 0 0)
                        , GHC.al_rest = []
                        , GHC.al_trailing = []
                        }
                      GHC.emptyComments
                  )
                  generatedSrcSpan
              )
              (commaSeparate items)
          )
    }

ieVar :: String -> GHC.IE GhcPs
ieVar =
  GHC.IEVar GHC.NoExtField
    . L generatedSrcSpanAnn
    . GHC.IEName GHC.NoExtField
    . L generatedSrcSpanAnn
    . mkRdrUnqual
    . mkVarOcc

{-|

* @x@
* @x, y@
* @x, y, z@
-}
commaSeparate :: [a] -> [GHC.LocatedA a]
commaSeparate [] =
  []
commaSeparate [x] =
  [L generatedSrcSpanAnn x]
commaSeparate (x : xs@(_ : _)) =
  let xs' = commaSeparate xs
  in L
      ( GHC.SrcSpanAnn
          ( GHC.EpAnn
              (movedAnchor 0 0)
              (GHC.AnnListItem [GHC.AddCommaAnn $ epaDelta 0 0])
              GHC.emptyComments
          )
          generatedSrcSpan
      )
      x
      : (xs' & ix 0 . l_loc . ann . _EpAnn . epAnn_entry . anchor_op .~ GHC.MovedAnchor (GHC.deltaPos 0 1))
