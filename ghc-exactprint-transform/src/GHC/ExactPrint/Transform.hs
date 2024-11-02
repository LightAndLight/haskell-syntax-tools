{-# LANGUAGE LambdaCase #-}

module GHC.ExactPrint.Transform
  ( addTrailingComma
  , addFieldToRecord
  , addFieldToRecordDataDefn
  , addItemToExplicitList

    -- * Imports
  , addImportToModule
  , ImportGroup (..)
  , importGroup_loc
  , importGroup_imports
  , groupImports
  , fromImportGroup
  , singletonImportGroup
  , consImportGroup
  , prependToImportGroup
  , appendToImportGroup
  )
where

import Control.Applicative ((<|>))
import Control.Lens.At (ix)
import Control.Lens.Cons (_Snoc, _head, _last)
import Control.Lens.Fold ((^?))
import Control.Lens.Getter (to, (^.))
import Control.Lens.Lens (Lens', lens)
import Control.Lens.Prism (_Just)
import Control.Lens.Review (review, (#))
import Control.Lens.Setter (over, (.~))
import Control.Lens.Tuple (_1, _2, _5)
import Control.Monad (guard)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import GHC
  ( AddEpAnn (..)
  , AnchorOperation (..)
  , AnnKeywordId (..)
  , AnnListItem (..)
  , BangType
  , ConDeclField (ConDeclField)
  , DeltaPos
  , EpAnn (EpAnn, EpAnnNotUsed)
  , EpaLocation (..)
  , FieldOcc (..)
  , GenLocated (..)
  , GhcPs
  , HsExpr (..)
  , HsFieldBind (..)
  , HsRecFields (HsRecFields)
  , LHsDecl
  , LHsExpr
  , LHsRecField
  , NoExtField (..)
  , SrcSpanAnn' (SrcSpanAnn)
  , SrcSpanAnnA
  , TrailingAnn (..)
  , addTrailingAnnToA
  , deltaPos
  , emptyComments
  , getLoc
  , mkFieldOcc
  , noSrcSpanA
  )
import GHC qualified
import GHC.ExactPrint.Syntax (movedAnchor)
import GHC.Lens
import GHC.Types.Name.Occurrence (mkVarOcc)
import GHC.Types.Name.Reader (mkRdrUnqual)
import GHC.Types.SrcLoc (generatedSrcSpan)
import Generics.SYB (gshow)
import Language.Haskell.GHC.ExactPrint.Utils (rdrName2String)

generatedAnchor :: GHC.Anchor
generatedAnchor = GHC.spanAsAnchor generatedSrcSpan

addFieldToRecord :: (String, HsExpr GhcPs) -> LHsExpr GhcPs -> Maybe (LHsExpr GhcPs)
addFieldToRecord (fieldName, fieldValue) (L loc expr) = do
  RecordConArgs ex con flds <- expr ^? _RecordCon
  guard $ isNothing (flds ^. rec_dotdot)

  let
    fields = flds ^. rec_flds
    flds' =
      HsRecFields
        ( case fields ^? _last of
            Nothing ->
              {- The record has no fields.

                * Put the field root one space after the `{`.
                * Put the field RHS one space after the `=`.
              -}
              [newField (deltaPos 0 1) (deltaPos 0 1)]
            Just lastField ->
              {- The record has fields.

                * Place a comma at the end of the current fields
                  * Use the same relative position as previous commas
                  * Fall back to the location of `{`, then `deltaPos 0 0`
                * Reuse the record field root and RHS deltas
              -}
              addTrailingComma (fromMaybe (deltaPos 0 0) $ openCDelta ex) fields
                ++ [newField (recFieldDelta lastField) (recFieldRhsDelta lastField)]
        )
        Nothing

  pure $ L loc (review _RecordCon $ RecordConArgs ex con flds')
  where
    newField :: DeltaPos -> DeltaPos -> LHsRecField GhcPs (LHsExpr GhcPs)
    newField newFieldDelta newFieldRhsDelta =
      L
        ( SrcSpanAnn
            ( EpAnn
                {-
                field = rhs
                \^
                -}
                generatedAnchor{GHC.anchor_op = MovedAnchor newFieldDelta}
                {-
                field = rhs
                           ^
                -}
                (AnnListItem [])
                emptyComments
            )
            generatedSrcSpan
        )
        HsFieldBind
          { hfbAnn =
              EpAnn
                {-
                field = rhs
                \^
                -}
                generatedAnchor
                {-
                field = rhs
                      ^

                It seems that only `AnnEqual` is accepted here.
                -}
                [AddEpAnn AnnEqual $ EpaDelta (deltaPos 0 1) []]
                emptyComments
          , hfbLHS =
              L noSrcSpanA . mkFieldOcc . L noSrcSpanA . mkRdrUnqual $ mkVarOcc fieldName
          , hfbRHS =
              L
                ( SrcSpanAnn
                    ( EpAnn
                        {-
                        field = rhs
                                ^
                        -}
                        generatedAnchor{GHC.anchor_op = MovedAnchor newFieldRhsDelta}
                        {-
                        field = rhs
                                   ^
                        -}
                        (AnnListItem [])
                        emptyComments
                    )
                    generatedSrcSpan
                )
                fieldValue
          , hfbPun = False
          }

addFieldToRecordDataDefn ::
  String -> (String, BangType GhcPs) -> LHsDecl GhcPs -> Maybe (LHsDecl GhcPs)
addFieldToRecordDataDefn targetName (fieldName, fieldTy) decl = do
  (_ext, lName, _tyVars, _fixity, dataDefn) <- decl ^? l_val . _TyClD . _2 . _DataDecl
  guard $ lName ^. l_val == _TyConName # targetName

  constructors <- dataDefn ^? dd_cons . _DataTypeCons_data
  constructor <- case constructors of [c] -> pure c; _ -> Nothing

  lfields <- constructor ^? l_val . _ConDeclH98 . con_args . _RecCon
  let fields = lfields ^. l_val

  let
    fieldRootDelta =
      fromMaybe
        (deltaPos 0 1)
        (fields ^? _last . l_loc . ann . _EpAnn . epAnn_entry . anchor_op . _MovedAnchor)
    fieldTypeDelta =
      fromMaybe
        (deltaPos 0 1)
        ( fields
            ^? _last . l_val . cd_fld_type . l_loc . ann . _EpAnn . epAnn_entry . anchor_op . _MovedAnchor
        )

  let
    mOpenBraceDelta = lfields ^? l_loc . ann . _EpAnn . epAnn_entry . anchor_op . _MovedAnchor

    fields' =
      addTrailingComma (fromMaybe (deltaPos 0 0) mOpenBraceDelta) fields
        <> [ L
              ( SrcSpanAnn
                  (EpAnn generatedAnchor{GHC.anchor_op = MovedAnchor fieldRootDelta} (AnnListItem []) emptyComments)
                  generatedSrcSpan
              )
              $ ConDeclField
                (EpAnn generatedAnchor [AddEpAnn AnnDcolon (EpaDelta (deltaPos 0 1) [])] emptyComments)
                [L noSrcSpanA . FieldOcc NoExtField . L noSrcSpanA . mkRdrUnqual $ mkVarOcc fieldName]
                ( L
                    ( SrcSpanAnn
                        (EpAnn generatedAnchor{GHC.anchor_op = MovedAnchor fieldTypeDelta} (AnnListItem []) emptyComments)
                        generatedSrcSpan
                    )
                    fieldTy
                )
                Nothing
           ]

    dataDefn' =
      dataDefn
        & dd_cons . _DataTypeCons_data
          .~ [ constructor
                & l_val . _ConDeclH98 . con_args . _RecCon . l_val
                  .~ fields'
             ]

  pure $
    decl
      & l_val . _TyClD . _2 . _DataDecl . _5 .~ dataDefn'

addTrailingComma ::
  DeltaPos ->
  [GenLocated SrcSpanAnnA e] ->
  [GenLocated SrcSpanAnnA e]
addTrailingComma _ [] = []
addTrailingComma commaDelta [L (SrcSpanAnn epa ss) field] =
  [ L
      ( SrcSpanAnn
          ( addTrailingAnnToA
              ss
              (AddCommaAnn $ EpaDelta commaDelta [])
              emptyComments
              epa
          )
          ss
      )
      field
  ]
addTrailingComma commaDelta (x : xs) =
  let commaDelta' =
        case GHC.ann $ getLoc x of
          EpAnnNotUsed -> commaDelta
          EpAnn _anc (AnnListItem trailings) _comments ->
            case mapMaybe (\case AddCommaAnn loc -> Just loc; _ -> Nothing) trailings of
              [] -> commaDelta
              loc : _ ->
                case loc of
                  EpaSpan{} -> commaDelta
                  EpaDelta delta _comments -> delta
  in x : addTrailingComma commaDelta' xs

openCDelta :: EpAnn [AddEpAnn] -> Maybe DeltaPos
openCDelta a = do
  anns <- a ^? _EpAnn . epAnn_anns
  let anns' = mapMaybe (\case AddEpAnn AnnOpenC loc -> Just loc; _ -> Nothing) anns
  anns' ^? _head . _EpaDelta . _1

recFieldDelta :: LHsRecField GhcPs (LHsExpr GhcPs) -> DeltaPos
recFieldDelta (L loc (HsFieldBind _ext _lhs _rhs _pun)) =
  case GHC.anchor_op . GHC.entry . GHC.ann $ loc of
    UnchangedAnchor -> error "recFieldDelta: UnchangedAnchor"
    MovedAnchor delta -> delta

recFieldRhsDelta :: LHsRecField GhcPs (LHsExpr GhcPs) -> DeltaPos
recFieldRhsDelta (L _ (HsFieldBind _ext _lhs rhs _pun)) =
  case GHC.anchor_op . GHC.entry . GHC.ann $ getLoc rhs of
    UnchangedAnchor -> error "recFieldRhsDelta: UnchangedAnchor"
    MovedAnchor delta -> delta

data ImportGroup = ImportGroup
  { _importGroup_loc :: GHC.EpAnn GHC.AnnListItem
  , _importGroup_imports :: NonEmpty (GHC.LImportDecl GhcPs)
  }

importGroup_loc :: Lens' ImportGroup (GHC.EpAnn GHC.AnnListItem)
importGroup_loc = lens _importGroup_loc (\x a -> x{_importGroup_loc = a})

importGroup_imports :: Lens' ImportGroup (NonEmpty (GHC.LImportDecl GhcPs))
importGroup_imports = lens _importGroup_imports (\x a -> x{_importGroup_imports = a})

fromImportGroup :: ImportGroup -> [GHC.LImportDecl GhcPs]
fromImportGroup (ImportGroup loc group) =
  NonEmpty.toList $ group & ix 0 . l_loc . ann .~ loc

singletonImportGroup :: GHC.LImportDecl GhcPs -> ImportGroup
singletonImportGroup imp =
  ImportGroup
    (imp ^. l_loc . ann)
    ( NonEmpty.singleton $
        imp
          & l_loc . ann . _EpAnn . epAnn_entry . anchor_op
            .~ MovedAnchor (deltaPos 0 0)
    )

consImportGroup ::
  GHC.LImportDecl GhcPs ->
  ImportGroup ->
  Either (ImportGroup, ImportGroup) ImportGroup
consImportGroup imp ig@(ImportGroup loc group) =
  let imp' = singletonImportGroup imp
  in case loc ^? _EpAnn . epAnn_entry . anchor_op . _MovedAnchor . to GHC.deltaLine of
      Just l
        | l > 1 ->
            Left (imp', ig)
      _ ->
        Right $
          ImportGroup
            (imp' ^. importGroup_loc)
            ((imp' ^. importGroup_imports) <> (group & ix 0 . l_loc . ann .~ loc))

prependToImportGroup ::
  GHC.ImportDecl GhcPs ->
  ImportGroup ->
  ImportGroup
prependToImportGroup imp (ImportGroup loc group) =
  ImportGroup
    loc
    ( L
        ( SrcSpanAnn
            ( EpAnn
                generatedAnchor{GHC.anchor_op = MovedAnchor $ deltaPos 0 0}
                (AnnListItem [])
                emptyComments
            )
            generatedSrcSpan
        )
        imp
        `NonEmpty.cons` ( group
                            & ix 0 . l_loc . ann . _EpAnn . epAnn_entry . anchor_op
                              .~ MovedAnchor (deltaPos 1 0)
                        )
    )

appendToImportGroup ::
  GHC.ImportDecl GhcPs ->
  ImportGroup ->
  ImportGroup
appendToImportGroup imp (ImportGroup loc group) =
  ImportGroup
    loc
    ( group
        <> NonEmpty.singleton
          ( L
              ( SrcSpanAnn
                  ( EpAnn
                      generatedAnchor{GHC.anchor_op = MovedAnchor $ deltaPos 1 0}
                      (AnnListItem [])
                      emptyComments
                  )
                  generatedSrcSpan
              )
              imp
          )
    )

-- | Group imports that are on consecutive lines.
groupImports :: [GHC.LImportDecl GhcPs] -> [ImportGroup]
groupImports [] = []
groupImports (imp : imps) =
  case groupImports imps of
    [] ->
      [singletonImportGroup imp]
    ig : rest ->
      case consImportGroup imp ig of
        Left (ig1, ig2) -> ig1 : ig2 : rest
        Right ig' -> ig' : rest

addImportToModule ::
  GHC.ImportDecl GhcPs -> GHC.Located (GHC.HsModule GhcPs) -> GHC.Located (GHC.HsModule GhcPs)
addImportToModule theImport =
  over
    (l_val . hsmodImports)
    ( \imps ->
        let
          grouped = groupImports imps
        in
          case grouped ^? _Snoc of
            Nothing ->
              [ L
                  ( SrcSpanAnn
                      ( EpAnn
                          generatedAnchor{GHC.anchor_op = MovedAnchor $ deltaPos 2 0}
                          (AnnListItem [])
                          emptyComments
                      )
                      generatedSrcSpan
                  )
                  theImport
              ]
            Just (groups, group) ->
              concatMap fromImportGroup $
                review _Snoc (groups, appendToImportGroup theImport group)
    )

addItemToExplicitList :: HsExpr GhcPs -> LHsExpr GhcPs -> Maybe (LHsExpr GhcPs)
addItemToExplicitList item (L loc expr) =
  emptyList <|> explicitList
  where
    emptyList = do
      (GHC.NoExtField, L (GHC.SrcSpanAnn epAnn _srcSpan) var) <- expr ^? _HsVar
      guard $ rdrName2String var == "[]"

      let
        items =
          [ L
              ( GHC.SrcSpanAnn (GHC.EpAnn (movedAnchor 0 0) (GHC.AnnListItem []) GHC.emptyComments) generatedSrcSpan
              )
              item
          ]

        ext =
          fmap
            ( \case
                GHC.NameAnnOnly GHC.NameSquare open close trailing ->
                  GHC.AnnList
                    { GHC.al_anchor = Nothing
                    , GHC.al_open = Just $ GHC.AddEpAnn GHC.AnnOpenS open
                    , GHC.al_close = Just $ GHC.AddEpAnn GHC.AnnCloseS close
                    , GHC.al_rest = []
                    , GHC.al_trailing = trailing
                    }
                nameAnn -> error $ "expected NameAnnOnly NameSquare, got: " <> gshow nameAnn
            )
            epAnn

      pure $
        L
          loc
          (review _ExplicitList (ext, items))

    explicitList = do
      (ex, items) <- expr ^? _ExplicitList

      let
        items' =
          case items ^? _last of
            Nothing ->
              -- The list has no items. Put the item immediately after the `[`.
              [ L
                  ( GHC.SrcSpanAnn (GHC.EpAnn (movedAnchor 0 0) (GHC.AnnListItem []) GHC.emptyComments) generatedSrcSpan
                  )
                  item
              ]
            Just lastField ->
              {- The list has items.

                * Place a comma at the end of the current items
                  * Use the same relative position as previous commas
                  * Fall back to the location of `[`, then `deltaPos 0 0`
                * Reuse the previous item delta
              -}
              let
                mCloseBracketDelta =
                  ex
                    ^? _EpAnn
                      . epAnn_anns
                      . al_close
                      . _Just
                      . _AddEpAnn GHC.AnnCloseS
                      . _EpaDelta
                      . _1

                newCommaDelta =
                  case (loc ^? ann . _EpAnn . epAnn_entry . anchor_op . _MovedAnchor, mCloseBracketDelta) of
                    -- When the ']' is on a new line, put the comma on a new line too
                    (Just exprDelta, Just closeBracketDelta)
                      | let l = GHC.getDeltaLine closeBracketDelta
                      , l > 0 ->
                          deltaPos 1 (GHC.deltaColumn exprDelta)
                    _ ->
                      deltaPos 0 0
              in
                addTrailingComma newCommaDelta items
                  ++ [L (lastField ^. l_loc) item]

      pure $ L loc (review _ExplicitList (ex, items'))
