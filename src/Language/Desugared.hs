module Language.Desugared (
  Module(..),
  Decl(..),
  Exp(..),
  Pat(..),
  Literal(..),
  Alt(..),
  Absyn.Annotated(..),
  module Syntax.Common
) where

import qualified Language.Absyn as Absyn
import Util

import Syntax.Common

import Syntax.Type (Constraints)
import Data.List ((\\), nub)
import Language.Haskell.Exts (ModulePragma)

-- | A complete Desugared source module.
data Module l
    = Module l (Maybe (Absyn.ModuleHead l)) [Absyn.ModulePragma l] [Absyn.ImportDecl l] [Decl l]
    -- ^ an ordinary Desugared module
  deriving (Eq,Ord,Show,Functor)

getDecls :: Module l -> [Decl l]
getDecls (Module l _ _ _ decls) = decls

-- | A top-level declaration.
data Decl l
     = PatBind      l (Pat l) (Exp l)
     -- ^ A pattern binding
  deriving (Eq,Ord,Show,Functor)

-- | A pattern, to be matched against a value.
data Pat l
    = PVar l (Name l)                       -- ^ variable
    | PLit l (Absyn.Sign l) (Literal l)     -- ^ literal constant
    | PWildCard l                           -- ^ wildcard pattern: @_@
    | PTuple l [Pat l]                      -- ^ tuple pattern
    | PList l [Pat l]                       -- ^ list pattern
    | PApp l (QName l) [Pat l]              -- ^ Pattern application
    | PInfixApp l (Pat l) (QName l) (Pat l) -- ^ Pattern application
  deriving (Eq,Ord,Show,Functor)

-- | Desugared expressions.
data Exp l
    = Var l (QName l)                       -- ^ variable
    | Lit l (Literal l)                     -- ^ literal constant
    | App l (Exp l) (Exp l)                 -- ^ ordinary application
    | Lambda l (Pat l) (Exp l)              -- ^ lambda expression
    | Tuple l [Exp l]                       -- ^ tuple
    | List l [Exp l]                        -- ^ list
    | If l (Exp l) (Exp l) (Exp l)          -- ^ @if@ /exp/ @then@ /exp/ @else@ /exp/
    -- | Let l (Pat l) (Exp l) (Exp l)         -- ^ let-binding
    | Case l (Exp l) [Alt l]                -- ^ @case@ /exp/ @of@ /alts/
    -- | Paren l (Exp l)                       -- ^ parenthesised expression
    -- Versioned expressions
    | VRes l Label (Exp l)
    | VExt l (Exp l)
    -- LambdaCase
    -- | LCase l [Alt l]                       -- ^ @\case@ /alts/
  deriving (Eq,Ord,Show,Functor)

-- | An /alt/ alternative in a @case@ expression.
data Alt l
    = Alt l (Pat l) (Exp l)
  deriving (Eq,Ord,Show,Functor)

instance HasName (Pat l) where
  getName (PVar _ name) = getName name
  getName _             = "Patterns without PVar do not have a name field."

instance Absyn.Annotated Module where
  ann (Module l _ _ _ _) = l
  amap f (Module l mmh pragmas iss dcls) = Module (f l) mmh pragmas iss dcls

instance Absyn.Annotated Decl where
  ann (PatBind l _ _) = l
  amap f (PatBind l p exp) = PatBind (f l) p exp

instance Absyn.Annotated Pat where
  ann p = case p of
    PVar l _           -> l
    PLit l _ _         -> l
    PWildCard l        -> l
    PTuple l _         -> l
    PList l _          -> l
    PApp l _ _         -> l
    PInfixApp l _ _ _  -> l
  amap f p1 = case p1 of
    PVar l n            -> PVar (f l) n
    PLit l sg lit       -> PLit (f l) sg lit
    PWildCard l         -> PWildCard (f l)
    PTuple l ps         -> PTuple (f l) ps
    PList l ps          -> PList (f l) ps
    PApp l qn ps        -> PApp (f l) qn ps
    PInfixApp l p qn ps -> PInfixApp (f l) p qn ps

instance Absyn.Annotated Exp where
  ann e = case e of
    Var l _                -> l
    Lit l _                -> l
    App l _ _              -> l
    Lambda l _ _           -> l
    If l _ _ _             -> l
    Tuple l _              -> l
    List l _               -> l
    -- Let l _ _ _            -> l
    VRes l _ _             -> l
    VExt l _               -> l

  amap f e1 = case e1 of
    Var l qn        -> Var (f l) qn
    Lit l lit       -> Lit (f l) lit
    App l e1' e2    -> App (f l) e1' e2
    Lambda l ps e   -> Lambda (f l) ps e
    Tuple l exps    -> Tuple (f l) exps
    List l exps     -> List (f l) exps
    If l ec et ee   -> If (f l) ec et ee
    -- Let l p e1 e2   -> Let (f l) p e1 e2
    VRes l vbs e    -> VRes (f l) vbs e
    VExt l e        -> VExt (f l) e


instance HasVar (Exp l) where
  freeVars exp =
    case exp of
      var@(Var l1 (UnQual l2 (Ident l3 str)))
                    -> [str]
      var@(Var _ _) -> []
      lit@(Lit _ _) -> []
      App _ e1 e2   -> freeVars e1 ++ freeVars e2
      Tuple _ lst   -> concatMap freeVars lst
      List _ lst    -> concatMap freeVars lst
      If _ e1 e2 e3 -> freeVars e1 ++ freeVars e2 ++ freeVars e3
      Lambda _ p e  -> freeVars e \\ boundVars p
      VRes _ vbs e  -> freeVars e
      VExt _ e      -> freeVars e
  freeVars' exp =
    case exp of
      var@(Var l1 (UnQual l2 (Ident l3 str)))
                    -> [str]
      var@(Var _ _) -> []
      lit@(Lit _ _) -> []
      App _ e1 e2   -> freeVars' e1 ++ freeVars' e2
      Tuple _ lst   -> concatMap freeVars' lst
      List _ lst    -> concatMap freeVars' lst
      If _ e1 e2 e3 -> freeVars' e1 ++ freeVars' e2 ++ freeVars' e3
      Lambda _ p e  -> freeVars' e \\ boundVars p
      VRes _ vbs e  -> freeVars' e
      VExt _ e      -> []
  vars exp = case exp of
      var@(Var l1 (UnQual l2 (Ident l3 str)))
                    -> [str]
      var@(Var _ _) -> []
      lit@(Lit _ _) -> []
      App _ e1 e2   -> vars e1 ++ vars e2
      Tuple _ lst   -> concatMap vars lst
      List _ lst    -> concatMap vars lst
      If _ e1 e2 e3 -> vars e1 ++ vars e2 ++ vars e3
      Lambda _ p e  -> vars e
      VRes _ vbs e  -> vars e
      VExt _ e      -> vars e

instance HasVar (Decl l) where
  freeVars (PatBind _ pat e) = freeVars e
  freeVars' (PatBind _ pat e) = freeVars' e
  vars (PatBind _ pat e) = vars e

instance HasVar (Module l) where
  freeVars (Module _ _ _ _ ds) = nub $ concatMap freeVars ds
  freeVars' (Module _ _ _ _ ds) = nub $ concatMap freeVars' ds
  vars (Module _ _ _ _ ds) = nub $ concatMap vars ds

boundVarsPats :: [Pat l] -> [String]
boundVarsPats = foldl (\acc p -> boundVars p ++ acc) []

boundVars :: Pat l -> [String]
boundVars (PVar _ name) = [getName name]
boundVars (PLit {})  = []
boundVars (PWildCard _) = []

---------------------
--  Pretty printer --
---------------------

instance PrettyAST (Module SrcSpanInfo) where
  ppE (Module srcLocInfo moduleHead pragmas importDecl decl) =
        nest 2 $ parens $ ppE "Module" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE moduleHead <> line
    <+> pplist ppE importDecl <> line
    <+> pplist ppE decl
  ppP (Module srcLocInfo moduleHead pragmas importDecl decl) =
        -- ppP "module" <+> 
        ppP moduleHead <+> ppP "where" <> line <> concatWith (surround line) (map ppP decl)

instance PrettyAST (Decl SrcSpanInfo) where
  ppE (PatBind srcLocInfo pat rhd) =
        nest 2 $ parens $ ppE "PatBind" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE pat <> line
    <+> ppE rhd
  ppP (PatBind srcLocInfo pat rhd) = ppP pat <+> ppP "=" <+> ppP rhd <> line

instance PrettyAST (Exp SrcSpanInfo) where
  ppE (Var srcLocInfo qname) =
        nest 2 $ parens $ ppE "Var" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE qname
  ppE (Lit srcLocInfo literal) =
        nest 2 $ parens $ ppE "Lit" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE literal
  ppE (App srcLocInfo exp1 exp2) =
        nest 2 $ parens $ ppE "App" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE exp1 <> line
    <+> ppE exp2
  -- ppE (NegApp srcLocInfo exp) =
  --       nest 2 $ parens $ ppE "NegApp" <> line
  --   <+> ppE srcLocInfo <> line
  --   <+> ppE exp
  ppE (If srcLocInfo exp1 exp2 exp3) =
        nest 2 $ parens $ ppE "If" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE exp1 <> line
    <+> ppE exp2 <> line
    <+> ppE exp3
  ppE (Lambda srcLocInfo pat exp) =
        nest 2 $ parens $ ppE "Lambda" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE pat <> line
    <+> ppE exp
  ppE (Tuple srcLocInfo elms) = 
        nest 2 $ parens $ ppE "Tuple" <> line
    <+> ppE srcLocInfo <> line
    <+> parens (concatWith (surround $ comma <> space) $ map ppE elms)
  ppE (List srcLocInfo elms) =
        nest 2 $ parens $ ppE "List" <> line
    <+> ppE srcLocInfo <> line
    <+> brackets (concatWith (surround $ comma <> space) $ map ppE elms)
  ppE (Case srcLocInfo exp alts) =
        nest 2 $ parens $ ppE "Case" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE exp <> line
    <+> concatWith (surround $ line <+> comma) (map ppE alts)
  -- ppE (Let srcLocInfo pat exp1 exp2) = 
  --       nest 2 $ parens $ ppE "Let" <> line
  --   <+> ppE srcLocInfo <> line
  --   <+> ppE pat <> line
  --   <+> ppE exp1 <> line
  --   <+> ppE exp2
  ppE (VRes srcLocInfo vbs e) =
        nest 2 $ parens $ ppE "VRes" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE vbs <> line
    <+> ppE e
  ppE (VExt srcLocInfo e) =
        nest 2 $ parens $ ppE "VExt" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE e
  ppP (Var _ qname) = ppP qname
  ppP (Lit _ literal) = ppP literal
  ppP (Tuple _ elms) = parens $ concatWith (surround $ comma <> space) $ map ppP elms 
  ppP (List _ elms) = brackets $ concatWith (surround $ comma <> space) $ map ppP elms 
  ppP (App _ e1 e2) = parens $ ppP e1 <+> ppP e2
  ppP (If _ e1 e2 e3) = parens $ ppP "if" <+> ppP e1 <+> ppP "then"  <+> ppP e2 <+> ppP "else" <+> ppP e3
  ppP (Lambda _ p e) = parens $ backslash <> ppP p <> dot <> ppP e
  ppP (Case _ e alts) = parens $ ppP "case" <+> ppP e <+> ppP "of" <+> braces (concatWith (surround $ semicolon <> space) (map ppP alts))
  -- ppP (Let _ p e1 e2) = parens $ ppP "Let" <+> ppP p <+> ppP "="  <+> ppP e1 <+> ppP "in" <+> ppP e2
  ppP (VRes _ vbs e) = parens $ ppP "version" <+> ppP vbs <+> ppP "of" <+> ppP e
  ppP (VExt _ e) = parens $ ppP "unversion" <+> ppP e

instance PrettyAST (Pat SrcSpanInfo) where
  ppE (PVar srcLocInfo name) =
        nest 2 $ parens $ ppE "PVar" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE name
  ppE (PLit srcLocInfo sign literal) =
        nest 2 $ parens $ ppE "PLit" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE sign <> line
    <+> ppE literal
  ppE (PWildCard srcLocInfo) =
        nest 2 $ parens $ ppE "PWildCard" <> line
    <+> ppE srcLocInfo
  ppE (PTuple srcLocInfo pats) =
        nest 2 $ parens $ ppE "PTuple" <> line
    <+> ppE srcLocInfo
    <+> brackets (concatWith (surround $ line <> comma <> space) (map ppE pats))
  ppE (PList srcLocInfo pats) =
        nest 2 $ parens $ ppE "PList" <> line
    <+> ppE srcLocInfo
    <+> brackets (concatWith (surround $ line <> comma <> space) (map ppE pats))
  ppE (PApp srcLocInfo qn pats) =
        nest 2 $ parens $ ppE "PApp" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE qn <> line
    <+> brackets (concatWith (surround $ line <> comma <> space) (map ppE pats))
  ppE (PInfixApp srcLocInfo p1 qn p2) =
        nest 2 $ parens $ ppE "PInfixApp" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE p1 <> line
    <+> ppE qn <> line
    <+> ppE p2
  ppP (PVar _ name) = ppP name
  ppP (PLit _ sign literal) = ppP sign <> ppP literal
  ppP (PWildCard _) = ppP "_"
  ppP (PTuple _ pats) = parens $ concatWith (surround $ comma <> space) (map ppP pats)
  ppP (PList _ pats) = brackets $ concatWith (surround $ comma <> space) (map ppP pats)
  ppP (PApp _ qn pats) = parens $ ppP qn <+> concatWith (surround space) (map ppP pats)
  ppP (PInfixApp _ p1 qn p2) = parens $ ppP p1 <+> ppP qn <+> ppP p2

instance PrettyAST (Alt SrcSpanInfo) where
  ppE (Alt srcLocInfo p e) =
        nest 2 $ parens $ ppE "Alt" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE p <> line
    <+> ppE e <> line
  ppP (Alt _ p e) = ppP p <+> ppP "->" <+> ppP e