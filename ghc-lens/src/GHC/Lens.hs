{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module GHC.Lens
  ( -- * AST

    -- ** ConDecl

    -- | c.f. 'GHC.ConDecl'
    _ConDeclH98
  , ConDeclH98Args (..)
  , con_args

    -- ** ConDeclField

    -- | c.f. 'GHC.ConDeclField'
  , cd_fld_type

    -- ** DataDefnCons

    -- | c.f. 'GHC.DataDefnCons'
  , _DataTypeCons_data

    -- ** GRHS

    -- | c.f. 'GHC.GRHS'
  , _GRHS
  , GRHSArgs (..)
  , grhs_body

    -- ** GRHSs

    -- | c.f. 'GHC.GRHSs'
  , grhssGRHSs

    -- ** GenLocated

    -- | c.f. 'GHC.GenLocated'
  , l_loc
  , l_val

    -- ** HsBindLR

    -- | c.f. 'GHC.HsBindLR'
  , _FunBind
  , FunBindArgs (..)
  , fun_id
  , fun_matches

    -- ** HsConDetails

    -- | c.f. 'GHC.HsConDetails'
  , _RecCon

    -- ** HsDataDefn

    -- | c.f. 'GHC.HsDataDefn'
  , dd_cons

    -- ** HsDecl

    -- | c.f. 'GHC.HsDecl'
  , _TyClD
  , _ValD
  , ValDArgs (..)
  , valD_bind

    -- ** HsExpr

    -- | c.f. 'GHC.Expr'
  , _RecordCon
  , RecordConArgs (..)

    -- ** HsModule

    -- | c.f. 'GHC.HsModule'
  , hsmodDecls
  , hsmodImports

    -- ** HsRecFields

    -- | c.f. 'GHC.HsRecFields'
  , rec_flds
  , rec_dotdot

    -- ** LHsQTyVars

    -- | c.f. 'LHsQTyVars'
  , hsq_explicit

    -- ** HsTyVarBndr

    -- | c.f. 'GHC.HsTyVarBndr'
  , tyVarBndrVar

    -- ** Match

    -- | c.f. 'GHC.Match'
  , m_grhss

    -- ** MatchGroup

    -- | c.f. 'GHC.MatchGroup'
  , mg_alts

    -- ** OccName

    -- | c.f. 'GHC.OccName'
  , _TcOcc

    -- ** RdrName

    -- | c.f. 'GHC.RdrName'
  , _Unqual
  , _TyConName

    -- ** TyClDecl

    -- | c.f. 'GHC.tyClDecl'
  , _DataDecl

    -- * Exact-print annotations

    -- ** Anchor

    -- | c.f. 'GHC.Anchor'
  , anchor_op

    -- ** AnchorOperation

    -- | c.f. 'GHC.AnchorOperation'
  , _MovedAnchor

    -- ** AnnList

    -- | c.f. 'GHC.AnnList'
  , al_open

    -- ** EpAnn

    -- | c.f. 'GHC.EpAnn'
  , _EpAnn
  , EpAnnArgs (..)
  , epAnn_entry
  , epAnn_anns

    -- ** EpaLocation

    -- | c.f. 'GHC.EpaLocation'
  , _EpaDelta

    -- ** SrcSpanAnn'

    -- | c.f. 'GHC.SrcSpanAnn''
  , ann
  )
where

import Control.Lens
import GHC qualified
import GHC.Types.Name.Occurrence (OccName, isTcOcc, mkTcOcc, occNameString)
import GHC.Types.Var (Specificity)

_EpaDelta :: Prism' GHC.EpaLocation (GHC.DeltaPos, [GHC.LEpaComment])
_EpaDelta = prism' (uncurry GHC.EpaDelta) (\case GHC.EpaDelta a b -> Just (a, b); _ -> Nothing)

_MovedAnchor :: Prism' GHC.AnchorOperation GHC.DeltaPos
_MovedAnchor = prism' GHC.MovedAnchor (\case GHC.MovedAnchor a -> Just a; _ -> Nothing)

anchor_op :: Lens' GHC.Anchor GHC.AnchorOperation
anchor_op = lens GHC.anchor_op (\x a -> x{GHC.anchor_op = a})

data EpAnnArgs ann = EpAnnArgs
  { _epAnn_entry :: !GHC.Anchor
  , _epAnn_anns :: !ann
  , _epAnn_comments :: !GHC.EpAnnComments
  }

epAnn_entry :: Lens' (EpAnnArgs ann) GHC.Anchor
epAnn_entry = lens _epAnn_entry (\x a -> x{_epAnn_entry = a})

epAnn_anns :: Lens (EpAnnArgs ann) (EpAnnArgs ann') ann ann'
epAnn_anns = lens _epAnn_anns (\x a -> x{_epAnn_anns = a})

_EpAnn :: Prism (GHC.EpAnn a) (GHC.EpAnn b) (EpAnnArgs a) (EpAnnArgs b)
_EpAnn =
  prism
    (\(EpAnnArgs a b c) -> GHC.EpAnn a b c)
    (\case GHC.EpAnn a b c -> Right (EpAnnArgs a b c); GHC.EpAnnNotUsed -> Left GHC.EpAnnNotUsed)

ann :: Lens (GHC.SrcSpanAnn' a) (GHC.SrcSpanAnn' b) a b
ann = lens GHC.ann (\x a -> x{GHC.ann = a})

cd_fld_type :: Lens' (GHC.ConDeclField pass) (GHC.LBangType pass)
cd_fld_type = lens GHC.cd_fld_type (\x a -> x{GHC.cd_fld_type = a})

data ConDeclH98Args pass = ConDeclH98Args
  { _con_ext :: GHC.XConDeclH98 pass
  , _con_name :: GHC.LIdP pass
  , _con_forall :: Bool
  , _con_ex_tvs :: [GHC.LHsTyVarBndr Specificity pass]
  , _con_mb_cxt :: Maybe (GHC.LHsContext pass)
  , _con_args :: GHC.HsConDeclH98Details pass
  , _con_doc :: Maybe (GHC.LHsDoc pass)
  }

_ConDeclH98 :: Prism' (GHC.ConDecl pass) (ConDeclH98Args pass)
_ConDeclH98 =
  prism'
    (\(ConDeclH98Args a b c d e f g) -> GHC.ConDeclH98 a b c d e f g)
    ( \case
        GHC.ConDeclH98 a b c d e f g -> Just (ConDeclH98Args a b c d e f g)
        _ -> Nothing
    )

con_args :: Lens' (ConDeclH98Args pass) (GHC.HsConDeclH98Details pass)
con_args = lens _con_args (\x a -> x{_con_args = a})

_RecCon ::
  Prism
    (GHC.HsConDetails tyarg arg rec)
    (GHC.HsConDetails tyarg arg rec')
    rec
    rec'
_RecCon =
  prism
    GHC.RecCon
    ( \case
        GHC.PrefixCon a b -> Left (GHC.PrefixCon a b)
        GHC.RecCon a -> Right a
        GHC.InfixCon a b -> Left (GHC.InfixCon a b)
    )

_DataTypeCons_data :: Prism' (GHC.DataDefnCons a) [a]
_DataTypeCons_data =
  prism' (GHC.DataTypeCons False) (\case GHC.DataTypeCons False a -> Just a; _ -> Nothing)

dd_cons :: Lens' (GHC.HsDataDefn pass) (GHC.DataDefnCons (GHC.LConDecl pass))
dd_cons = lens GHC.dd_cons (\x a -> x{GHC.dd_cons = a})

l_loc :: Lens (GHC.GenLocated l a) (GHC.GenLocated l' a) l l'
l_loc = lens (\(GHC.L l _) -> l) (\(GHC.L _ a) l' -> GHC.L l' a)

l_val :: Lens (GHC.GenLocated l a) (GHC.GenLocated l b) a b
l_val = lens (\(GHC.L _ a) -> a) (\(GHC.L l _) b -> GHC.L l b)

hsmodDecls :: Lens' (GHC.HsModule p) [GHC.LHsDecl p]
hsmodDecls = lens GHC.hsmodDecls (\x a -> x{GHC.hsmodDecls = a})

hsmodImports :: Lens' (GHC.HsModule p) [GHC.LImportDecl p]
hsmodImports = lens GHC.hsmodImports (\x a -> x{GHC.hsmodImports = a})

data ValDArgs p = ValDArgs {_valD_ext :: GHC.XValD p, _valD_bind :: GHC.HsBind p}

valD_bind :: Lens' (ValDArgs p) (GHC.HsBind p)
valD_bind = lens _valD_bind (\x a -> x{_valD_bind = a})

_ValD :: Prism' (GHC.HsDecl p) (ValDArgs p)
_ValD =
  prism' (\(ValDArgs a b) -> GHC.ValD a b) (\case GHC.ValD a b -> Just $ ValDArgs a b; _ -> Nothing)

_TyClD :: Prism' (GHC.HsDecl p) (GHC.XTyClD p, GHC.TyClDecl p)
_TyClD = prism' (uncurry GHC.TyClD) (\case GHC.TyClD a b -> Just (a, b); _ -> Nothing)

_DataDecl ::
  Prism'
    (GHC.TyClDecl pass)
    (GHC.XDataDecl pass, GHC.LIdP pass, GHC.LHsQTyVars pass, GHC.LexicalFixity, GHC.HsDataDefn pass)
_DataDecl =
  prism'
    (\(a, b, c, d, e) -> GHC.DataDecl a b c d e)
    (\case GHC.DataDecl a b c d e -> Just (a, b, c, d, e); _ -> Nothing)

data FunBindArgs idL idR = FunBindArgs
  { _fun_ext :: GHC.XFunBind idL idR
  , _fun_id :: GHC.LIdP idL
  , _fun_matches :: GHC.MatchGroup idR (GHC.LHsExpr idR)
  }

fun_id :: Lens' (FunBindArgs idL idR) (GHC.LIdP idL)
fun_id = lens _fun_id (\x a -> x{_fun_id = a})

fun_matches :: Lens' (FunBindArgs idL idR) (GHC.MatchGroup idR (GHC.LHsExpr idR))
fun_matches = lens _fun_matches (\x a -> x{_fun_matches = a})

_FunBind :: Prism' (GHC.HsBindLR idL idR) (FunBindArgs idL idR)
_FunBind =
  prism'
    (\(FunBindArgs a b c) -> GHC.FunBind a b c)
    (\case GHC.FunBind a b c -> Just (FunBindArgs a b c); _ -> Nothing)

mg_alts :: Lens' (GHC.MatchGroup p body) (GHC.XRec p [GHC.LMatch p body])
mg_alts = lens GHC.mg_alts (\x a -> x{GHC.mg_alts = a})

m_grhss :: Lens' (GHC.Match p body) (GHC.GRHSs p body)
m_grhss = lens GHC.m_grhss (\x a -> x{GHC.m_grhss = a})

grhssGRHSs :: Lens' (GHC.GRHSs p body) [GHC.LGRHS p body]
grhssGRHSs = lens GHC.grhssGRHSs (\x a -> x{GHC.grhssGRHSs = a})

data GRHSArgs p body = GRHSArgs
  { _grhs_ext :: GHC.XCGRHS p body
  , _grhs_guards :: [GHC.GuardLStmt p]
  , _grhs_body :: body
  }

grhs_body :: Lens' (GRHSArgs p body) body
grhs_body = lens _grhs_body (\x a -> x{_grhs_body = a})

_GRHS :: Prism' (GHC.GRHS p body) (GRHSArgs p body)
_GRHS =
  prism'
    (\(GRHSArgs a b c) -> GHC.GRHS a b c)
    (\case GHC.GRHS a b c -> Just (GRHSArgs a b c); _ -> Nothing)

data RecordConArgs p = RecordConArgs
  { _rcon_ext :: GHC.XRecordCon p
  , _rcon_con :: GHC.XRec p (GHC.ConLikeP p)
  , _rcon_flds :: GHC.HsRecordBinds p
  }

_RecordCon :: Prism' (GHC.HsExpr p) (RecordConArgs p)
_RecordCon =
  prism'
    (\(RecordConArgs a b c) -> GHC.RecordCon a b c)
    (\case GHC.RecordCon a b c -> Just (RecordConArgs a b c); _ -> Nothing)

rec_flds ::
  Lens
    (GHC.HsRecFields p arg)
    (GHC.HsRecFields p arg')
    [GHC.LHsRecField p arg]
    [GHC.LHsRecField p arg']
rec_flds = lens GHC.rec_flds (\x a -> x{GHC.rec_flds = a})

rec_dotdot :: Lens' (GHC.HsRecFields p arg) (Maybe (GHC.XRec p GHC.RecFieldsDotDot))
rec_dotdot = lens GHC.rec_dotdot (\x a -> x{GHC.rec_dotdot = a})

_Unqual :: Prism' GHC.RdrName OccName
_Unqual = prism' GHC.Unqual (\case GHC.Unqual a -> Just a; _ -> Nothing)

_TcOcc :: Prism' OccName String
_TcOcc = prism' mkTcOcc (\a -> if isTcOcc a then Just $ occNameString a else Nothing)

_TyConName :: Prism' GHC.RdrName String
_TyConName = _Unqual . _TcOcc

hsq_explicit :: Lens' (GHC.LHsQTyVars pass) [GHC.LHsTyVarBndr () pass]
hsq_explicit = lens GHC.hsq_explicit (\x a -> x{GHC.hsq_explicit = a})

tyVarBndrVar ::
  GHC.XXTyVarBndr pass ~ GHC.DataConCantHappen => Lens' (GHC.HsTyVarBndr flag pass) (GHC.LIdP pass)
tyVarBndrVar =
  lens
    ( \case
        GHC.UserTyVar _ext _flag name -> name
        GHC.KindedTyVar _ext _flag name _kind -> name
    )
    ( \x name ->
        case x of
          GHC.UserTyVar ext flag _ -> GHC.UserTyVar ext flag name
          GHC.KindedTyVar ext flag _ kind -> GHC.KindedTyVar ext flag name kind
    )

al_open :: Lens' GHC.AnnList (Maybe GHC.AddEpAnn)
al_open = lens GHC.al_open (\x a -> x{GHC.al_open = a})
