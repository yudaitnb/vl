module Language.LambdaVL (
  module Syntax.Common,
  Module(..),
  Decl(..),
  Exp(..),
  Pat(..),
  Alt(..),
  Literal(..),
  Absyn.Annotated(..),
  getDecls, getBind,
  splitDeclToMap, splitDeclsToMap,
  freeVars'
) where

import qualified Language.Absyn as Absyn
import Data.List ((\\))
import Data.Map (Map, fromList, empty, union)

import Syntax.Common

import Util


-- | A complete Lambda VL source module.
data Module l
    = Module l (Maybe (Absyn.ModuleHead l)) [Absyn.ModulePragma l] [Absyn.ImportDecl l] [Decl l]
  deriving (Eq,Ord,Show,Functor)

getDecls :: Module l -> [Decl l]
getDecls (Module l _ _ _ decls) = decls

splitDeclToMap :: Decl l -> Map VarName (Exp l)
splitDeclToMap d@(PatBind _ p e) = fromList [(getName p, e)]

splitDeclsToMap :: [Decl l] -> Map VarName (Exp l)
splitDeclsToMap = foldr (\d acc -> acc `union` splitDeclToMap d) empty

-- | A top-level declaration.
data Decl l
     = PatBind      l (Pat l) (Exp l)
     -- ^ A pattern binding
  deriving (Ord,Show,Functor)

getBind :: Decl l -> Exp l
getBind (PatBind _ _ e) = e

instance Eq l => Eq (Decl l) where
  PatBind _ p e == PatBind _ p' e' = p == p' && e == e'

-- | A pattern, to be matched against a value.
data Pat l
    = PVar l (Name l)                       -- ^ variable
    | PLit l (Absyn.Sign l) (Literal l)     -- ^ literal constant
    | PWildCard l                           -- ^ wildcard pattern: @_@
    | PBox l (Pat l)                        -- ^ promoted pattern; @[@/x/@]@ -> ...
    | PTuple l [Pat l]                      -- ^ tuple pattern
    | PList l [Pat l]                       -- ^ list pattern
    | PApp l (QName l) [Pat l]              -- ^ Infix pattern application
    | PInfixApp l (Pat l) (QName l) (Pat l) -- ^ Pattern application
  deriving (Eq,Ord,Show,Functor)

-- | An /alt/ alternative in a @case@ expression.
data Alt l
    = Alt l (Pat l) (Exp l)
  deriving (Eq,Ord,Show,Functor)

-- | Lambda VL expressions.
data Exp l
    = Var l (QName l)                       -- ^ variable
    | Lit l (Literal l)                     -- ^ literal constant
    | App l (Exp l) (Exp l)                 -- ^ ordinary application
    -- | NegApp l (Exp l)                      -- ^ negation expression @-/exp/@ (unary minus)
    | Lambda l (Pat l) (Exp l)              -- ^ lambda expression
    | CLet l (Pat l) (Exp l) (Exp l)        -- ^ contextual let
    | Tuple l [Exp l]                       -- ^ tuple
    | List l [Exp l]                        -- ^ list
    | Con l (QName l)                       -- ^ constructor
    -- | Let l (Pat l) (Exp l) (Exp l)         -- ^ contexutual let-binding
    | If l (Exp l) (Exp l) (Exp l)          -- ^ @if@ /exp/ @then@ /exp/ @else@ /exp/
    -- Versioned expressions
    | Pr l (Exp l)                          -- ^ An expression promoted version resources
    | Case l (Exp l) [Alt l]                -- ^ @case@ /exp/ @of@ /alts/
    | VRes l Label (Exp l)
    | VExt l (Exp l)
  deriving (Ord,Show,Functor)

instance Eq l => Eq (Exp l) where
  Var _ qn1 == Var _ qn2 = qn1 == qn2
  Lit _ l1 == Lit _ l2 = l1 == l2
  App _ e1 e2 == App _ e1' e2'= e1 == e1' && e2 == e2'
  Lambda _ p e == Lambda _ p' e' = e == e' && p == p'
  CLet _ p e1 e2 == CLet _ p' e1' e2' = p == p' && e1 == e1' && e2 == e2'
  Tuple _ xs1 == Tuple _ xs2 = and $ zipWith (==) xs1 xs2
  List _ xs1 == List _ xs2 = and $ zipWith (==) xs1 xs2
  Con _ qn1 == Con _ qn2 = qn1 == qn2
  If _ e1 e2 e3 == If _ e1' e2' e3' = e1 == e1' && e2 == e2' && e3 == e3'
  Pr _ e == Pr _ e' = e == e'
  VRes _ l e == VRes _ l' e' = l == l' && e == e'
  VExt _ e == VExt _ e' = e == e'
  _ == _ = False

instance HasName (Decl l) where
  getName (PatBind _ p _) = getName p

instance HasName (Exp l) where
  getName exp = case exp of
    Var _ qn  -> getName qn
    Lit _ lit -> getName lit
    _         -> error "Expressions without Var/Literal do not have a name field."

instance HasName (Pat l) where
  getName (PVar _ name) = getName name
  getName _             = error "Patterns without PVar do not have a name field."

--

instance HasVar (Exp l) where
  freeVars exp =
    case exp of
      Var _ qn      -> case qn of
        UnQual _ n  -> [UQVar (getName n)]
        Qual _ mn n -> [QVar (getName mn) (getName n)]
        Special _ s -> []
      lit@(Lit _ _) -> []
      App _ e1 e2   -> freeVars e1 ++ freeVars e2
      If _ e1 e2 e3 -> freeVars e1 ++ freeVars e2 ++ freeVars e3
      Lambda _ p e  -> filter (`notElem` boundVars p) (freeVars e)
      CLet _ p e e' -> filter (`notElem` boundVars p) (freeVars e) ++ freeVars e'
      Tuple _ elms  -> concatMap freeVars elms
      List _ elms   -> concatMap freeVars elms
      Case _ e alts -> freeVars e ++ concatMap freeVars alts
      Pr _ e        -> freeVars e
      Con _ qn      -> case qn of
        UnQual _ n  -> [UQVar (getName n)]
        Qual _ mn n -> [QVar (getName mn) (getName n)]
        Special _ s -> []
      VRes _ vbs e  -> freeVars e
      VExt _ e      -> freeVars e
  vars exp = case exp of
      Var _ qn      -> case qn of
        UnQual _ n  -> [UQVar (getName n)]
        Qual _ mn n -> [QVar (getName mn) (getName n)]
        Special _ s -> []
      lit@(Lit _ _) -> []
      App _ e1 e2   -> vars e1 ++ vars e2
      If _ e1 e2 e3 -> vars e1 ++ vars e2 ++ vars e3
      Lambda _ p e  -> vars e
      CLet _ p e e' -> vars e ++ vars e'
      Tuple _ elms  -> concatMap vars elms
      List _ elms   -> concatMap vars elms
      Case _ e alts -> vars e ++ concatMap vars alts
      Pr _ e        -> vars e
      Con _ qn      -> case qn of
        UnQual _ n  -> [UQVar (getName n)]
        Qual _ mn n -> [QVar (getName mn) (getName n)]
        Special _ s -> []
      VRes _ vbs e  -> vars e
      VExt _ e      -> vars e

instance HasVar (Alt l) where
  freeVars (Alt _ p e) = filter (`notElem` boundVars p) (freeVars e)
  vars (Alt _ p e) = vars e

-- considering VExt
freeVars' :: Exp l -> [VarKey]
freeVars' exp =
  case exp of
    Var _ qn      -> case qn of
      UnQual _ n  -> [UQVar $ getName n]
      Qual _ mn n -> [QVar (getName mn) (getName n)]
      Special _ s -> []
    lit@(Lit _ _) -> []
    App _ e1 e2   -> freeVars' e1 ++ freeVars' e2
    If _ e1 e2 e3 -> freeVars' e1 ++ freeVars' e2 ++ freeVars' e3
    Lambda _ p e  -> freeVars' e \\ boundVars p
    CLet _ p e e' -> (freeVars' e \\ boundVars p) ++ freeVars' e'
    Tuple _ elms  -> concatMap freeVars' elms
    List _ elms   -> concatMap freeVars' elms
    Case _ e alts -> freeVars' e ++ concatMap freeVarsAlt' alts
    Pr _ e        -> freeVars' e
    Con _ qn      -> case qn of
      UnQual _ n  -> [UQVar $ getName n]
      Qual _ mn n -> [QVar (getName mn) (getName n)]
      Special _ s -> []
    VRes _ vbs e  -> freeVars' e
    VExt _ e      -> []

freeVarsAlt' :: Alt l -> [VarKey]
freeVarsAlt' (Alt _ p e) = freeVars' e \\ boundVars p

-- boundVarsPats :: [Pat l] -> [VarName]
-- boundVarsPats = foldl (\acc p -> boundVars p ++ acc) []

boundVars :: Pat l -> [VarKey]
boundVars p = case p of
  PVar _ name          -> [UQVar (getName name)]
  PLit {}              -> []
  PWildCard _          -> []
  PTuple _ ps          -> concatMap boundVars ps
  PList _ ps           -> concatMap boundVars ps
  PApp _ _ ps          -> concatMap boundVars ps
  PInfixApp _ p1 qn p2 -> boundVars p1 ++ boundVars p2
  PBox _ p             -> boundVars p

-----------

instance Absyn.Annotated Module where
  ann (Module l _ _ _ _) = l
  amap f (Module l mmh pragmas iss dcls) = Module (f l) mmh pragmas iss dcls

instance Absyn.Annotated Decl where
  ann (PatBind l _ _) = l
  amap f (PatBind l p exp) = PatBind (f l) p exp

instance Absyn.Annotated Pat where
  ann (PVar l _) = l
  ann (PLit l _ _) = l
  ann (PWildCard l) = l
  ann (PBox l _) = l
  ann (PTuple l _) = l
  ann (PList l _) = l
  ann (PApp l _ _) = l
  amap f p1 = case p1 of
    PVar l n          -> PVar (f l) n
    PLit l sg lit     -> PLit (f l) sg lit
    PWildCard l       -> PWildCard (f l)
    PBox l p          -> PBox (f l) p

instance Absyn.Annotated Exp where
  ann e = case e of
    Var l _                -> l
    Lit l _                -> l
    App l _ _              -> l
    If l _ _ _             -> l
    Lambda l _ _           -> l
    CLet l _ _ _           -> l
    Tuple l _              -> l
    List l _               -> l
    Case l _ _             -> l
    Pr l _                 -> l
    Con l _                -> l
    VRes l _ _             -> l
    VExt l _               -> l

  amap f e1 = case e1 of
    Var l qn        -> Var (f l) qn
    Lit l lit       -> Lit (f l) lit
    App l e1' e2    -> App (f l) e1' e2
    -- NegApp l e      -> NegApp (f l) e
    Lambda l ps e   -> Lambda (f l) ps e
    If l ec et ee   -> If (f l) ec et ee
    CLet l p e e'     -> CLet (f l) p e e'
    Tuple l es      -> Tuple (f l) es
    List l es       -> List (f l) es
    Case l e alt    -> Case (f l) e alt
    Pr l e          -> Pr (f l) e
    Con l e         -> Con (f l) e
    VRes l vbs e    -> VRes (f l) vbs e
    VExt l e        -> VExt (f l) e

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
  ppE (PatBind srcLocInfo pat exp) =
        nest 2 $ parens $ ppE "PatBind" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE pat <> line
    <+> ppE exp
  ppP (PatBind srcLocInfo pat exp) = ppP pat <+> ppP "=" <+> ppP exp <> line

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
  ppE (Tuple srcLocInfo elms) = 
        nest 2 $ parens $ ppE "Tuple" <> line
    <+> ppE srcLocInfo <> line
    <+> parens (concatWith (surround $ comma <> space) $ map ppE elms)
  ppE (List srcLocInfo elms) =
        nest 2 $ parens $ ppE "List" <> line
    <+> ppE srcLocInfo <> line
    <+> brackets (concatWith (surround $ comma <> space) $ map ppE elms)
  ppE (Lambda srcLocInfo pat exp) =
        nest 2 $ parens $ ppE "Lambda" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE pat <> line
    <+> ppE exp
  ppE (Case srcLocInfo exp alts) =
        nest 2 $ parens $ ppE "Case" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE exp <> line
    <+> concatWith (surround $ line <+> comma) (map ppE alts)
  ppE (Pr srcLocInfo exp) =
        nest 2 $ parens $ ppE "Pr" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE exp
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
  ppP (App _ e1 e2) = parens $ ppP e1 <+> ppP e2
  ppP (Lambda _ p e) = parens $ backslash <> ppP p <> dot <> ppP e
  ppP (CLet _ p e e') = parens $ ppP "let" <+> ppP p <+> ppP "=" <+> ppP e <+> ppP "in" <+> ppP e'
  ppP (If _ e1 e2 e3) = parens $ ppP "If" <+> ppP e1 <+> ppP "then" <+> ppP e2 <+> ppP "else" <+> ppP e3
  ppP (Tuple _ elms) = parens $ concatWith (surround $ comma <> space) $ map ppP elms 
  ppP (List _ elms) = brackets $ concatWith (surround $ comma <> space) $ map ppP elms 
  ppP (Case _ e alts) = parens $ ppP "case" <+> ppP e <+> ppP "of" <+> braces (concatWith (surround $ semicolon <> space) (map ppP alts))
  ppP (Pr _ e) = brackets $ ppP e
  ppP (Con _ qn) = ppP qn
  ppP (VRes _ vbs e) = parens $ ppP "version" <+> ppP vbs <+> ppP "of" <+> ppP e
  ppP (VExt _ e) = parens $ ppP "unversion" <+> ppP e

instance PrettyAST (Alt SrcSpanInfo) where
  ppE (Alt srcLocInfo p e) =
        nest 2 $ parens $ ppE "Alt" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE p <> line
    <+> ppE e <> line
  ppP (Alt _ p e) = ppP p <+> ppP "->" <+> ppP e

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
  ppE (PBox srcLocInfo pat) =
        nest 2 $ parens $ ppE "PBox" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE pat
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
  ppP (PBox _ pat) = brackets $ ppP pat
  ppP (PList _ pats) = brackets $ concatWith (surround $ comma <> space) (map ppP pats)
  ppP (PApp _ qn pats) = parens $ ppP qn <+> concatWith (surround space) (map ppP pats)
  ppP (PInfixApp _ p1 qn p2) = parens $ ppP p1 <+> ppP qn <+> ppP p2