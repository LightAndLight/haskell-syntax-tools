{-# LANGUAGE OverloadedStrings #-}

module GHC.ExactPrint.Debug
  ( debugAddEpAnn
  , debugAnchor
  , debugAnnList
  , debugAnnListItem
  , debugConDecl
  , debugConDeclField
  , debugDataDefnCons
  , debugDeltaPos
  , debugEpaLocation
  , debugEpAnn
  , debugFieldOcc
  , debugGenLocated
  , debugHsConDeclH98Details
  , debugHsDataDefn
  , debugHsDecl
  , debugHsExpr
  , debugHsModule
  , debugHsScaled
  , debugLHsExpr
  , debugLHsQTyVars
  , debugLocatedNRdrName
  , debugSrcAnnNoEpAnns
  , debugSrcSpan
  , debugSrcSpanAnnA
  , debugSrcSpanAnnL
  , debugTrailingAnn
  , debugTyClDecl

    -- * Internals
  , debugList
  , debugMaybe
  , sexpr
  , todo
  )
where

import Data.Bool (bool)
import Data.Proxy (Proxy (..))
import Data.Void (absurd)
import GHC
import GHC.TypeLits (KnownSymbol, symbolVal)
import GHC.Types.Basic (Origin (..))
import GHC.Utils.Outputable hiding ((<>))
import GHC.Utils.Outputable qualified as SDoc

sexpr :: SDoc -> [SDoc] -> SDoc
sexpr name [] =
  parens name
sexpr name contents =
  text "(" SDoc.<> name $+$ nest 2 (foldr ($+$) empty contents SDoc.<> text ")")

todo :: Outputable a => a -> SDoc
todo x = text "TODO: " <+> ppr x

debugHsDecl :: HsDecl GhcPs -> SDoc
debugHsDecl decl =
  case decl of
    TyClD NoExtField decl' ->
      sexpr
        "TyClD"
        [ debugTyClDecl decl'
        ]
    SigD NoExtField sig ->
      sexpr
        "SigD"
        [ debugSig sig
        ]
    ValD NoExtField bind ->
      sexpr
        "ValD"
        [ debugHsBind bind
        ]
    _ -> text "TODO: " <+> ppr decl

debugHsBind :: HsBind GhcPs -> SDoc
debugHsBind = debugHsBindLR

debugHsBindLR :: HsBindLR GhcPs GhcPs -> SDoc
debugHsBindLR (FunBind NoExtField name matches) =
  sexpr
    "FunBind"
    [ debugLocatedNRdrName name
    , debugMatchGroup matches
    ]
debugHsBindLR a = sexpr "HsBindLR" [todo a]

debugMatchGroup :: MatchGroup GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs)) -> SDoc
debugMatchGroup (MG ext alts) =
  sexpr
    "MG"
    [ debugOrigin ext
    , debugGenLocated
        debugSrcSpanAnnL
        (debugList (debugGenLocated debugSrcSpanAnnA debugMatch))
        alts
    ]

debugMatch :: Match GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs)) -> SDoc
debugMatch (Match ext ctxt pats grhss) =
  sexpr
    "Match"
    [ debugEpAnn (debugList debugAddEpAnn) ext
    , debugHsMatchContext ctxt
    , debugList (debugGenLocated debugSrcSpanAnnA debugPat) pats
    , debugGRHSs grhss
    ]

debugGRHSs :: GRHSs GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs)) -> SDoc
debugGRHSs (GRHSs ext grhss localBinds) =
  sexpr
    "GRHSs"
    [ debugEpAnnComments ext
    , debugList (debugGenLocated debugSrcAnnNoEpAnns debugGRHS) grhss
    , debugHsLocalBinds localBinds
    ]

debugGRHS :: GRHS GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs)) -> SDoc
debugGRHS (GRHS ext guards body) =
  sexpr
    "GRHS"
    [ debugEpAnn debugGrhsAnn ext
    , debugList debugGuardLStmt guards
    , debugLHsExpr body
    ]

debugGuardLStmt :: GuardLStmt GhcPs -> SDoc
debugGuardLStmt =
  debugGenLocated debugSrcSpanAnnA debugStmtLR

debugStmtLR :: StmtLR GhcPs GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs)) -> SDoc
debugStmtLR a =
  sexpr
    "StmtLR"
    [ todo a
    ]

debugGrhsAnn :: GrhsAnn -> SDoc
debugGrhsAnn (GrhsAnn vb s) =
  sexpr
    "GrhsAnn"
    [ debugMaybe debugEpaLocation vb
    , debugAddEpAnn s
    ]

debugHsLocalBinds :: HsLocalBinds GhcPs -> SDoc
debugHsLocalBinds a =
  sexpr
    "HsLocalBinds"
    [ todo a
    ]

debugEpAnnComments :: EpAnnComments -> SDoc
debugEpAnnComments (EpaComments a) =
  sexpr
    "EpaComments"
    (debugLEpaComment <$> a)
debugEpAnnComments (EpaCommentsBalanced a b) =
  sexpr
    "EpaCommentsBalanced"
    [ debugList debugLEpaComment a
    , debugList debugLEpaComment b
    ]

debugLEpaComment :: LEpaComment -> SDoc
debugLEpaComment =
  debugGenLocated debugAnchor debugEpaComment

debugEpaComment :: EpaComment -> SDoc
debugEpaComment (EpaComment tok prior_tok) =
  sexpr
    "EpaComment"
    [ debugEpaCommentTok tok
    , debugRealSrcSpan prior_tok
    ]

debugEpaCommentTok :: EpaCommentTok -> SDoc
debugEpaCommentTok (EpaDocComment a) =
  sexpr
    "EpaDocComment"
    [ todo a
    ]
debugEpaCommentTok (EpaDocOptions a) =
  parens ("EpaDocOptions" <+> doubleQuotes (text a))
debugEpaCommentTok (EpaLineComment a) =
  parens ("EpaLineComment" <+> doubleQuotes (text a))
debugEpaCommentTok (EpaBlockComment a) =
  parens ("EpaBlockComment" <+> doubleQuotes (text a))
debugEpaCommentTok EpaEofComment =
  "EpaEofComment"

debugPat :: Pat GhcPs -> SDoc
debugPat a =
  sexpr
    "Pat"
    [ todo a
    ]

debugHsMatchContext :: HsMatchContext GhcPs -> SDoc
debugHsMatchContext (FunRhs fun fixity strictness) =
  sexpr
    "FunRhs"
    [ debugLocatedNRdrName fun
    , debugLexicalFixity fixity
    , debugSrcStrictness strictness
    ]
debugHsMatchContext a =
  sexpr
    "HsMatchContext"
    [ todo a
    ]

debugSrcStrictness :: SrcStrictness -> SDoc
debugSrcStrictness SrcLazy = "SrcLazy"
debugSrcStrictness SrcStrict = "SrcStrict"
debugSrcStrictness NoSrcStrict = "NoSrcStrict"

debugLexicalFixity :: LexicalFixity -> SDoc
debugLexicalFixity Prefix = "Prefix"
debugLexicalFixity Infix = "Infix"

debugOrigin :: Origin -> SDoc
debugOrigin FromSource = "FromSource"
debugOrigin Generated = "Generated "

debugSig :: Sig GhcPs -> SDoc
debugSig (TypeSig ext names rest) =
  sexpr
    "TypeSig"
    [ debugEpAnn debugAnnSig ext
    , debugList debugLocatedNRdrName names
    , debugLHsSigWcType rest
    ]
debugSig a = sexpr "Sig" [todo a]

debugLHsSigWcType :: LHsSigWcType GhcPs -> SDoc
debugLHsSigWcType (HsWC NoExtField body) =
  sexpr
    "HsWC"
    [ debugGenLocated debugSrcSpanAnnA debugHsSigType body
    ]

debugHsSigType :: HsSigType GhcPs -> SDoc
debugHsSigType (HsSig NoExtField bndrs body) =
  sexpr
    "HsSig"
    [ debugHsOutSigTyVarBndrs bndrs
    , debugGenLocated debugSrcSpanAnnA debugHsType body
    ]

debugHsType :: HsType GhcPs -> SDoc
debugHsType a =
  sexpr "HsType" [todo a]

debugHsOutSigTyVarBndrs :: HsOuterSigTyVarBndrs GhcPs -> SDoc
debugHsOutSigTyVarBndrs a =
  sexpr "HsOuterSigTyVarBndrs" [todo a]

debugAnnSig :: AnnSig -> SDoc
debugAnnSig (AnnSig dc rest) =
  sexpr
    "AnnSig"
    [ debugAddEpAnn dc
    , debugList debugAddEpAnn rest
    ]

debugTyClDecl :: TyClDecl GhcPs -> SDoc
debugTyClDecl decl =
  case decl of
    DataDecl ext tyCon tyVars _fixity dataDefn ->
      sexpr
        "DataDecl"
        [ debugEpAnn (debugList debugAddEpAnn) ext
        , debugLocatedNRdrName tyCon
        , debugLHsQTyVars tyVars
        , debugHsDataDefn dataDefn
        ]
    _ ->
      text "TODO: " <+> ppr decl

debugHsDataDefn :: HsDataDefn GhcPs -> SDoc
debugHsDataDefn (HsDataDefn NoExtField ctxt cType kindSig cons derivs) =
  sexpr
    "HsDataDefn"
    [ debugMaybe ((text "TODO: " <+>) . ppr) ctxt
    , debugMaybe ((text "TODO: " <+>) . ppr) cType
    , debugMaybe ((text "TODO: " <+>) . ppr) kindSig
    , debugDataDefnCons cons
    , text "TODO: " <+> ppr derivs
    ]

debugDataDefnCons :: DataDefnCons (LConDecl GhcPs) -> SDoc
debugDataDefnCons defn =
  case defn of
    DataTypeCons False a ->
      sexpr
        "DataTypeCons_data"
        [ debugList (debugGenLocated debugSrcSpanAnnA debugConDecl) a
        ]
    DataTypeCons True a ->
      sexpr
        "DataTypeCons_type"
        [ debugList (debugGenLocated debugSrcSpanAnnA debugConDecl) a
        ]
    NewTypeCon a ->
      sexpr
        "NewTypeCon"
        [ debugGenLocated debugSrcSpanAnnA debugConDecl a
        ]

debugConDecl :: ConDecl GhcPs -> SDoc
debugConDecl decl =
  case decl of
    ConDeclH98 ext name fa ex_tvs mb_cxt args doc ->
      sexpr
        "ConDeclH98"
        [ debugEpAnn (debugList debugAddEpAnn) ext
        , debugLocatedNRdrName name
        , bool (text "False") (text "True") fa
        , debugList (debugGenLocated debugSrcSpanAnnA ((text "TODO: " <+>) . ppr)) ex_tvs
        , debugMaybe ((text "TODO" <+>) . ppr) mb_cxt
        , debugHsConDeclH98Details args
        , debugMaybe ((text "TODO: " <+>) . ppr) doc
        ]
    _ ->
      text "TODO: " <+> ppr decl

debugHsConDeclH98Details :: HsConDeclH98Details GhcPs -> SDoc
debugHsConDeclH98Details details =
  case details of
    PrefixCon (x : _) _ ->
      absurd x
    PrefixCon [] b ->
      sexpr
        "PrefixCon"
        [ debugList debugHsScaled b
        ]
    RecCon a ->
      sexpr
        "RecCon"
        [ debugGenLocated debugSrcSpanAnnL (debugList $ debugGenLocated debugSrcSpanAnnA debugConDeclField) a
        ]
    InfixCon a b ->
      sexpr
        "InfixCon"
        [ debugHsScaled a
        , debugHsScaled b
        ]

debugHsScaled :: HsScaled GhcPs (LBangType GhcPs) -> SDoc
debugHsScaled (HsScaled a b) =
  sexpr
    "HsScaled"
    [ todo a
    , todo b
    ]

debugLHsQTyVars :: LHsQTyVars GhcPs -> SDoc
debugLHsQTyVars (HsQTvs NoExtField explicit) =
  sexpr
    "HsQTvs"
    [ debugList (debugGenLocated debugSrcSpanAnnA $ (text "TODO: " <+>) . ppr) explicit
    ]

debugConDeclField :: ConDeclField GhcPs -> SDoc
debugConDeclField (ConDeclField ext names ty doc) =
  sexpr
    "ConDeclField"
    [ debugEpAnn (debugList debugAddEpAnn) ext
    , debugList (debugGenLocated debugSrcAnnNoEpAnns debugFieldOcc) names
    , debugGenLocated debugSrcSpanAnnA todo ty
    , debugMaybe todo doc
    ]

debugFieldOcc :: FieldOcc GhcPs -> SDoc
debugFieldOcc fieldOcc =
  case fieldOcc of
    FieldOcc NoExtField label -> debugLocatedNRdrName label

debugSrcAnnNoEpAnns :: SrcAnn NoEpAnns -> SDoc
debugSrcAnnNoEpAnns (SrcSpanAnn x _) = debugEpAnn (\NoEpAnns -> text "NoEpAnns") x

debugGenLocated :: (l -> SDoc) -> (a -> SDoc) -> GenLocated l a -> SDoc
debugGenLocated debugL debugA (L l a) =
  sexpr "L" [debugL l, debugA a]

debugEpAnn :: (a -> SDoc) -> EpAnn a -> SDoc
debugEpAnn _ EpAnnNotUsed = text "EpAnnNotUsed"
debugEpAnn f (EpAnn anc a _comments) =
  sexpr "EpAnn" [debugAnchor anc, f a]

debugAnchor :: Anchor -> SDoc
debugAnchor anc =
  sexpr
    "Anchor"
    [ debugRealSrcSpan $ GHC.anchor anc
    , debugAnchorOperation $ GHC.anchor_op anc
    ]

debugAnchorOperation :: AnchorOperation -> SDoc
debugAnchorOperation UnchangedAnchor = "UnchangedAnchor"
debugAnchorOperation (MovedAnchor delta) = parens ("MovedAnchor" <+> debugDeltaPos delta)

debugRealSrcSpan :: RealSrcSpan -> SDoc
debugRealSrcSpan = doubleQuotes . ppr

debugDeltaPos :: DeltaPos -> SDoc
debugDeltaPos delta =
  parens (text "deltaPos" <+> ppr (getDeltaLine delta) <+> ppr (deltaColumn delta))

debugLHsExpr :: LHsExpr GhcPs -> SDoc
debugLHsExpr = debugGenLocated debugSrcSpanAnnA debugHsExpr

debugSrcSpan :: SrcSpan -> SDoc
debugSrcSpan = doubleQuotes . ppr

debugSrcSpanAnnA :: SrcSpanAnnA -> SDoc
debugSrcSpanAnnA (SrcSpanAnn epAnn _) = debugEpAnn debugAnnListItem epAnn

debugMaybe :: (a -> SDoc) -> Maybe a -> SDoc
debugMaybe _ Nothing = text "Nothing"
debugMaybe f (Just a) = sexpr "Just" [f a]

debugAddEpAnn :: AddEpAnn -> SDoc
debugAddEpAnn (AddEpAnn token location) =
  sexpr
    "AddEpAnn"
    [ ppr token
    , debugEpaLocation location
    ]

debugList :: (a -> SDoc) -> [a] -> SDoc
debugList _ [] = empty
debugList f (x : xs) = f x $+$ debugList f xs

debugAnnList :: AnnList -> SDoc
debugAnnList (AnnList anc open close rest trailing) =
  sexpr
    "AnnList"
    [ debugMaybe debugAnchor anc
    , debugMaybe debugAddEpAnn open
    , debugMaybe debugAddEpAnn close
    , debugList debugAddEpAnn rest
    , debugList debugTrailingAnn trailing
    ]

debugSrcSpanAnnL :: SrcSpanAnnL -> SDoc
debugSrcSpanAnnL (SrcSpanAnn epAnn _) = debugEpAnn debugAnnList epAnn

debugAnnListItem :: AnnListItem -> SDoc
debugAnnListItem (AnnListItem []) =
  parens "AnnListItem"
debugAnnListItem (AnnListItem items) =
  sexpr "AnnListItem" (fmap debugTrailingAnn items)

debugTrailingAnn :: TrailingAnn -> SDoc
debugTrailingAnn ta =
  parens
    ( text "TrailingAnn"
        <+> case ta of
          AddSemiAnn loc -> text "';'" <+> debugEpaLocation loc
          AddCommaAnn loc -> text "','" <+> debugEpaLocation loc
          AddVbarAnn loc -> text "'|'" <+> debugEpaLocation loc
    )

debugEpaLocation :: EpaLocation -> SDoc
debugEpaLocation location =
  case location of
    EpaSpan rss _ ->
      parens (text "EpaSpan" <+> doubleQuotes (ppr rss))
    EpaDelta delta _comments ->
      parens (text "EpaDelta" <+> debugDeltaPos delta)

debugLocatedNRdrName :: LocatedN RdrName -> SDoc
debugLocatedNRdrName (L (SrcSpanAnn epAnn _) a) =
  sexpr
    "RdrName"
    [ debugEpAnn (parens . ppr) epAnn
    , doubleQuotes $ ppr a
    ]

debugHsExpr :: HsExpr GhcPs -> SDoc
debugHsExpr expr =
  case expr of
    RecordCon ext con fields ->
      sexpr
        "RecordCon"
        [ debugEpAnn (debugList debugAddEpAnn) ext
        , debugLocatedNRdrName con
        , debugEpAnnsHsRecFields fields
        ]
    HsApp ext f x ->
      sexpr
        "HsApp"
        [ debugEpAnn (\NoEpAnns -> text "NoEpAnns") ext
        , debugLHsExpr f
        , debugLHsExpr x
        ]
    HsVar NoExtField x ->
      sexpr
        "HsVar"
        [ debugLocatedNRdrName x
        ]
    OpApp ext a b c ->
      sexpr
        "OpApp"
        [ debugEpAnn (debugList debugAddEpAnn) ext
        , debugLHsExpr a
        , debugLHsExpr b
        , debugLHsExpr c
        ]
    HsDo ext _doFlavour b ->
      sexpr
        "HsDo"
        [ debugEpAnn debugAnnList ext
        , debugGenLocated debugSrcSpanAnnL (debugList (debugGenLocated debugSrcSpanAnnA debugStmt)) b
        ]
    ExplicitList ext exprs ->
      sexpr
        "ExplicitList"
        [ debugEpAnn debugAnnList ext
        , debugList debugLHsExpr exprs
        ]
    _ -> text "TODO:" <+> ppr expr
  where
    debugStmt :: Stmt GhcPs (LHsExpr GhcPs) -> SDoc
    debugStmt stmt =
      case stmt of
        BodyStmt NoExtField body NoExtField NoExtField ->
          sexpr
            "BodyStmt"
            [ debugLHsExpr body
            ]
        _ ->
          text "TODO:" <+> ppr stmt

    debugEpAnnsHsRecFields :: HsRecordBinds GhcPs -> SDoc
    debugEpAnnsHsRecFields (HsRecFields fields _dotdot) =
      sexpr "HsRecFields" $
        fmap (debugGenLocated debugSrcSpanAnnA debugEpAnnsHsRecField) fields

    debugSrcSpanNoEpAnns :: SrcAnn NoEpAnns -> SDoc
    debugSrcSpanNoEpAnns (SrcSpanAnn x _) = debugEpAnn (\NoEpAnns -> text "NoEpAnns") x

    debugEpAnnsHsRecField :: HsRecField GhcPs (LHsExpr GhcPs) -> SDoc
    debugEpAnnsHsRecField (HsFieldBind epAnn lhs rhs _pun) =
      sexpr
        "HsFieldBind"
        [ debugEpAnn (debugList debugAddEpAnn) epAnn
        , debugGenLocated debugSrcSpanNoEpAnns debugFieldOcc lhs
        , debugLHsExpr rhs
        ]

debugHsModule :: HsModule GhcPs -> SDoc
debugHsModule (HsModule ext name exports imports decls) =
  sexpr
    "HsModule"
    [ debugXModulePs ext
    , debugMaybe (debugGenLocated debugSrcSpanAnnA debugModuleName) name
    , debugMaybe
        ( debugGenLocated
            debugSrcSpanAnnL
            (debugList (debugGenLocated debugSrcSpanAnnA debugIE))
        )
        exports
    , debugList (debugGenLocated debugSrcSpanAnnA debugImportDecl) imports
    , debugList (debugGenLocated debugSrcSpanAnnA debugHsDecl) decls
    ]

debugImportDecl :: ImportDecl GhcPs -> SDoc
debugImportDecl a =
  sexpr "ImportDecl" [todo a]

debugIE :: IE GhcPs -> SDoc
debugIE (IEVar NoExtField name) =
  sexpr
    "IEVar"
    [debugGenLocated debugSrcSpanAnnA debugIEWrappedName name]
debugIE x = todo x

debugIEWrappedName :: IEWrappedName GhcPs -> SDoc
debugIEWrappedName (IEName NoExtField name) =
  sexpr "IEName" [debugLocatedNRdrName name]
debugIEWrappedName a = todo a

debugModuleName :: ModuleName -> SDoc
debugModuleName = doubleQuotes . text . moduleNameString

debugXModulePs :: XModulePs -> SDoc
debugXModulePs (XModulePs ann layout _deprec _haddock) =
  sexpr
    "XModulePs"
    [ debugEpAnn debugAnnsModule ann
    , debugLayoutInfo layout
    ]

debugAnnsModule :: AnnsModule -> SDoc
debugAnnsModule (AnnsModule main decl) =
  sexpr
    "AnnsModule"
    [ debugList debugAddEpAnn main
    , debugAnnList decl
    ]

debugLayoutInfo :: LayoutInfo GhcPs -> SDoc
debugLayoutInfo (ExplicitBraces a b) =
  sexpr
    "ExplicitBraces"
    [ debugGenLocated debugTokenLocation debugHsToken a
    , debugGenLocated debugTokenLocation debugHsToken b
    ]
debugLayoutInfo (VirtualBraces n) = parens ("VirtualBraces" <+> ppr n)
debugLayoutInfo NoLayoutInfo = "NoLayoutInfo"

debugHsToken :: KnownSymbol tok => HsToken tok -> SDoc
debugHsToken (HsTok @tok) =
  sexpr "HsTok" [doubleQuotes $ text (symbolVal (Proxy :: Proxy tok))]

debugTokenLocation :: TokenLocation -> SDoc
debugTokenLocation NoTokenLoc = "NoTokenLoc"
debugTokenLocation (TokenLoc loc) =
  sexpr "TokenLoc" [debugEpaLocation loc]
