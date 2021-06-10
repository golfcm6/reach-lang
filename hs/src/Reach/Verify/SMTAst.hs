module Reach.Verify.SMTAst (
  SMTTrace(..),
  BindingOrigin(..),
  TheoremKind(..),
  SMTLet(..),
  SMTExpr(..),
  SynthExpr(..),
  SMTCat(..),
  SMTVal(..)) where

import Reach.AST.Base
import Reach.AST.DLBase
import Reach.Texty
import Reach.AddCounts
import Reach.CollectCounts
import qualified Data.ByteString as B
import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.Map as M
import Data.List (partition)


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
  | SMTMapSet DLVar DLArg (Maybe DLArg) -- Context
  | SMTMapRef DLVar DLVar               -- Context
  deriving (Eq, Show)

instance PrettySubst SynthExpr where
  prettySubst = \case
    SMTMapNew -> return "new Map()"
    SMTMapFresh -> return "Map?"
    SMTMapSet m f ma -> do
      m' <- prettySubst $ DLA_Var m
      f' <- prettySubst f
      ma' <- prettySubst ma
      return $ m' <> brackets f' <+> "=" <+> ma'
    SMTMapRef m f -> do
      m' <- prettySubst $ DLA_Var m
      f' <- prettySubst $ DLA_Var f
      return $ m' <> brackets f'

data SMTExpr
  = SMTModel BindingOrigin
  | SMTProgram DLExpr
  | SMTSynth SynthExpr
  deriving (Eq)

instance PrettySubst SMTExpr where
  prettySubst = \case
    SMTModel bo -> return $ viaShow bo
    SMTProgram de -> prettySubst de
    SMTSynth se -> prettySubst se

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

instance PrettySubst SMTLet where
  prettySubst = \case
    SMTLet _ dv _ Context se -> do
      se' <- prettySubst se
      env <- ask
      let wouldBe x = "  //    ^ would be " <> viaShow x
      let info = maybe "" wouldBe (M.lookup dv env)
      return $ "  const" <+> viaShow dv <+> "=" <+> se' <> ";" <> hardline <> info
    SMTLet at dv _ Witness se -> do
      env <- ask
      se' <- prettySubst se
      let wouldBe x = "  //    ^ could = " <> x <> hardline <> "          from:" <+> pretty at
      let info = maybe "" wouldBe (M.lookup dv env)
      return $ "  const" <+> viaShow dv <+> "=" <+> se' <> ";" <> hardline <> info
    SMTNop _ -> return ""

data SMTTrace
  = SMTTrace [SMTLet] TheoremKind DLVar
  deriving (Eq, Show)

instance Pretty SMTTrace where
  pretty = runIdentity . flip runReaderT mempty . prettySubst

instance PrettySubst SMTTrace where
  prettySubst (SMTTrace lets tk dv) = do
    let (inlinable, others) = partition isWitness lets
    inlinable' <- mapM (prettySubst . toWitness) inlinable
    lets' <- mapM prettySubst others
    return $
      "  // Violation Witness" <> hardline <> hardline <>
      concatWith (surround hardline) inlinable' <> hardline <> hardline <>
      "  // Theorem Formalization" <> hardline <> hardline <>
      concatWith (surround hardline) lets' <> hardline <>
      "  " <> pretty tk <> parens (pretty dv) <> ";" <> hardline
    where
      -- xxx nail down how to get witness variables
      isWitness = \case
        SMTLet _ _ _ _ (SMTProgram (DLE_Arg _ DLA_Var {})) -> False
        SMTLet _ _ _ _ (SMTProgram DLE_Arg {}) -> True
        SMTLet _ _ _ _ (SMTProgram e) -> not $ isPure e
        _ -> False
      toWitness = \case
        SMTLet a b c _ e -> SMTLet a b c Witness e
        ow -> ow

data SMTVal
  = SMV_Bool Bool
  | SMV_Int Int
  | SMV_Address SLPart
  | SMV_Null
  | SMV_Bytes B.ByteString
  | SMV_Array DLType [SMTVal]
  deriving (Eq, Show)

instance Pretty SMTVal where
  pretty = \case
    SMV_Bool b -> pretty $ DLL_Bool b
    SMV_Int i -> pretty i
    SMV_Address p -> pretty p
    SMV_Null -> "null"
    SMV_Bytes b -> pretty b
    SMV_Array t xs -> "array" <> parens (hsep $ punctuate comma [pretty t, brackets $ hsep $ punctuate comma $ map pretty xs])

instance Countable SynthExpr where
  counts = \case
    SMTMapNew -> mempty
    SMTMapFresh -> mempty
    SMTMapSet m f ma -> counts m <> counts f <> counts ma
    SMTMapRef m f -> counts m <> counts f

instance Countable SMTExpr where
  counts = \case
    SMTModel {} -> mempty
    SMTSynth s -> counts s
    SMTProgram de -> counts de

instance AC SMTLet where
  ac = \case
    SMTLet at dv x c se -> do
      x' <- ac_vdef (canDupe se) x
      case (isPure se, x') of
        (True, DLV_Eff) -> return $ SMTNop at
        _ -> do
          ac_visit se
          return $ SMTLet at dv x' c se
    SMTNop at -> return $ SMTNop at

instance AC SMTTrace where
  ac (SMTTrace lets tk dv) = do
    ac_visit dv
    lets' <- ac $ reverse lets
    return $ SMTTrace (reverse $ filter dropNop lets') tk dv
    where
      dropNop = \case
        SMTNop _ -> False
        _ -> True
