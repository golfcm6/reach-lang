module Reach.Verify.SMTAst (
  SMTTrace(..),
  BindingOrigin(..),
  TheoremKind(..),
  SMTLet(..),
  SMTExpr(..),
  SynthExpr(..),
  SMTCat(..)) where

import Reach.AST.Base
import Reach.AST.DLBase
import Reach.Texty


data BindingOrigin
  = O_Join SLPart Bool
  | O_Msg SLPart (Maybe DLArg)
  | O_ClassJoin SLPart
  | O_ToConsensus
  | O_BuiltIn
  | O_Var
  | O_Interact
  | O_Expr DLExpr
  | O_Assignment
  | O_SwitchCase SLVar
  | O_ReduceVar
  | O_Export
  deriving (Eq)

instance Show BindingOrigin where
  show bo =
    case bo of
      O_Join who False -> "a dishonest join from " ++ sp who
      O_Msg who Nothing -> "a dishonest message from " ++ sp who
      O_Join who True -> "an honest join from " ++ sp who
      O_Msg who (Just what) -> "an honest message from " ++ sp who ++ " of " ++ sp what
      O_ClassJoin who -> "a join by a class member of " <> sp who
      O_ToConsensus -> "a consensus transfer"
      O_BuiltIn -> "builtin"
      O_Var -> "function return"
      O_Interact -> "interaction"
      O_Expr e -> "evaluating " ++ sp e
      O_Assignment -> "loop variable"
      O_SwitchCase vn -> "switch case " <> vn
      O_ReduceVar -> "map reduction"
      O_Export -> "export"
    where
      sp :: Pretty a => a -> String
      sp = show . pretty

instance IsPure BindingOrigin where
  isPure = \case
    O_Expr de -> isPure de
    O_ReduceVar -> True
    O_Export -> True
    -- Rest are `False` to be conservative with the lack of info
    O_Join {} -> False
    O_Msg {} -> False
    O_ClassJoin _ -> False
    O_ToConsensus -> False
    O_BuiltIn -> False
    O_Var -> False
    O_Interact -> False
    O_Assignment -> False
    O_SwitchCase _ -> False

data TheoremKind
  = TClaim ClaimType
  | TInvariant Bool
  | TWhenNotUnknown
  deriving (Eq, Show)

instance Pretty TheoremKind where
  pretty = \case
    TClaim c -> pretty c
    TInvariant False -> "while invariant before loop"
    TInvariant True -> "while invariant after loop"
    TWhenNotUnknown -> "when is not unknown"

data SMTCat
  = Witness
  | Context
  deriving (Eq, Show)

instance Pretty SMTCat where
  pretty = viaShow

data SynthExpr
  = SMTMapNew                           -- Context
  | SMTMapFresh                         -- Witness
  | SMTMapSet DLVar DLVar (Maybe DLArg) -- Context
  | SMTMapRef DLVar DLVar               -- Context
  deriving (Eq, Show)

instance Pretty SynthExpr where
  pretty = \case
    SMTMapNew -> "new Map()"
    SMTMapFresh -> "Map?"
    SMTMapSet m f ma ->
      pretty m <> brackets (pretty f) <+> "=" <+> pretty ma
    SMTMapRef m f -> pretty m <> brackets (pretty f)

data SMTExpr
  = SMTModel BindingOrigin
  | SMTProgram DLExpr
  | SMTSynth SynthExpr
  deriving (Eq)

instance Pretty SMTExpr where
  pretty = \case
    SMTModel bo -> viaShow bo
    SMTProgram de -> pretty de
    SMTSynth se -> pretty se

instance Show SMTExpr where
  show = \case
    SMTProgram dl -> show . pretty $ dl
    ow -> show ow

instance CanDupe SMTExpr where
  canDupe = \case
    SMTProgram de -> canDupe de
    _ -> True

instance IsPure SMTExpr where
  isPure = \case
    SMTProgram de -> isPure de
    _ -> True

data SMTLet
  = SMTLet SrcLoc DLVar DLLetVar SMTCat SMTExpr
  | SMTNop SrcLoc
  deriving (Eq, Show)

instance Ord SMTLet where
  compare (SMTLet _ ldv _ _ _) (SMTLet _ rdv _ _ _) = compare ldv rdv
  compare _ _ = LT

instance Pretty SMTLet where
  pretty (SMTLet _at dv _ _ se) =
    "  const" <+> viaShow dv <+> "=" <+> pretty se <> ";" -- <> hardline <>
    -- "// bound at:" <+> pretty at
  pretty (SMTNop _) = ""

data SMTTrace
  = SMTTrace [SMTLet] TheoremKind DLVar
  deriving (Eq, Show)

instance Pretty SMTTrace where
  pretty (SMTTrace lets tk dv) =
    concatWith (surround hardline) (map pretty lets) <> hardline <>
    "  " <> pretty tk <> parens (pretty dv) <> ";" <> hardline

data SMTVal
  = SMV_Bool Bool
  | SMV_Int Int
  | SMV_Address SLPart
  deriving (Eq)

