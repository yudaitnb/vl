module Syntax.LambdaVL (
  Module(..),
  Decl(..),
  Exp(..),
  Pat(..),
  Literal(..),
  HasVar(..),
  Absyn.Annotated(..),
  getDecls, getBind
) where

import qualified Syntax.Absyn as Absyn
import Data.List

import Syntax.Literal
import Syntax.Name
import Util
import Syntax.SrcLoc
import Syntax.Label


-- | A complete Lambda VL source module.
data Module l
    = Module l (Maybe (Absyn.ModuleHead l)) [Absyn.ImportDecl l] [Decl l]
  deriving (Eq,Ord,Show,Functor)

getDecls :: Module l -> [Decl l]
getDecls (Module l _ _ decls) = decls

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
    | PLit l (Absyn.Sign l) (Literal l)           -- ^ literal constant
    | PWildCard l                           -- ^ wildcard pattern: @_@
    | PBox l (Pat l)                        -- ^ promoted pattern; @[@/x/@]@ -> ...
  deriving (Eq,Ord,Show,Functor)

-- | Lambda VL expressions.
data Exp l
    = Var l (QName l)                       -- ^ variable
    | Lit l (Literal l)                     -- ^ literal constant
    | App l (Exp l) (Exp l)                 -- ^ ordinary application
    -- | NegApp l (Exp l)                      -- ^ negation expression @-/exp/@ (unary minus)
    | Lambda l (Pat l) (Exp l)              -- ^ lambda expression
    -- | Let l (Pat l) (Exp l) (Exp l)         -- ^ contexutual let-binding
    | If l (Exp l) (Exp l) (Exp l)          -- ^ @if@ /exp/ @then@ /exp/ @else@ /exp/
    -- Versioned expressions
    | Pr l (Exp l)                          -- ^ An expression promoted version resources
    -- | Case l (Exp l) [Alt l]                -- ^ @case@ /exp/ @of@ /alts/
    -- | Paren l (Exp l)                       -- ^ parenthesised expression
    -- | LCase l [Alt l]                       -- ^ @\case@ /alts/
    -- | VRes l Label (Exp l)
    | VRes l Label (Exp l)
    | VExt l (Exp l)
  deriving (Ord,Show,Functor)

instance Eq l => Eq (Exp l) where
  Var _ qn1 == Var _ qn2 = qn1 == qn2
  Lit _ l1 == Lit _ l2 = l1 == l2
  App _ e1 e2 == App _ e1' e2'= e1 == e1' && e2 == e2'
  Lambda _ p e == Lambda _ p' e' = e == e' && p == p'
  If _ e1 e2 e3 == If _ e1' e2' e3' = e1 == e1' && e2 == e2' && e3 == e3'
  Pr _ e == Pr _ e' = e == e'
  VRes _ l e == VRes _ l' e' = l == l' && e == e'
  VExt _ e == VExt _ e' = e == e'

instance HasName (Decl l) where
  getName (PatBind _ p _) = getName p

instance HasName (Exp l) where
  getName (Var _ qName) = getName qName
  getName (Lit _ literal) = getName literal
  getName _ = error "Expressions without Var/Literal do not have a name field."

instance HasName (Pat l) where
  getName (PVar _ name) = getName name
  getName _             = error "Patterns without PVar do not have a name field."

--

instance HasVar (Exp l) where
  freeVars exp =
    case exp of
      var@(Var l1 (UnQual l2 (Ident l3 str)))
                    -> [str]
      var@(Var _ _) -> []
      lit@(Lit _ _) -> []
      App _ e1 e2   -> freeVars e1 ++ freeVars e2
      If _ e1 e2 e3 -> freeVars e1 ++ freeVars e2 ++ freeVars e3
      Lambda _ p e  -> filter (`notElem` boundVars p) (freeVars e)
      Pr _ e        -> freeVars e
      VRes _ vbs e  -> freeVars e
      VExt _ e      -> freeVars e
  freeVars' exp =
    case exp of
      var@(Var l1 (UnQual l2 (Ident l3 str)))
                    -> [str]
      var@(Var _ _) -> []
      lit@(Lit _ _) -> []
      App _ e1 e2   -> freeVars' e1 ++ freeVars' e2
      If _ e1 e2 e3 -> freeVars' e1 ++ freeVars' e2 ++ freeVars' e3
      Lambda _ p e  -> freeVars' e \\ boundVars p
      Pr _ e        -> freeVars' e
      VRes _ vbs e  -> freeVars' e
      VExt _ e      -> []
  vars exp = case exp of
      var@(Var l1 (UnQual l2 (Ident l3 str)))
                    -> [str]
      var@(Var _ _) -> []
      lit@(Lit _ _) -> []
      App _ e1 e2   -> vars e1 ++ vars e2
      If _ e1 e2 e3 -> vars e1 ++ vars e2 ++ vars e3
      Lambda _ p e  -> vars e
      Pr _ e        -> vars e
      VRes _ vbs e  -> vars e
      VExt _ e      -> vars e

-- boundVarsPats :: [Pat l] -> [String]
-- boundVarsPats = foldl (\acc p -> boundVars p ++ acc) []

boundVars :: Pat l -> [String]
boundVars p = case p of
  PVar _ name -> [getName name]
  PLit {}     -> []
  PWildCard _ -> []
  PBox _ p    -> boundVars p

-----------

instance Absyn.Annotated Module where
  ann (Module l _ _ _) = l
  amap f (Module l mmh iss dcls) = Module (f l) mmh iss dcls

instance Absyn.Annotated Decl where
  ann (PatBind l _ _) = l
  amap f (PatBind l p exp) = PatBind (f l) p exp

instance Absyn.Annotated Pat where
  ann (PVar l _) = l
  ann (PLit l _ _) = l
  ann (PWildCard l) = l
  ann (PBox l p) = l
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
    Lambda l _ _           -> l
    If l _ _ _             -> l
    -- Let l _ _ _            -> l
    Pr l _                 -> l
    VRes l _ _             -> l
    VExt l _               -> l

  amap f e1 = case e1 of
    Var l qn        -> Var (f l) qn
    Lit l lit       -> Lit (f l) lit
    App l e1' e2    -> App (f l) e1' e2
    -- NegApp l e      -> NegApp (f l) e
    Lambda l ps e   -> Lambda (f l) ps e
    If l ec et ee   -> If (f l) ec et ee
    -- Let l p e1 e2   -> Let (f l) p e1 e2
    Pr l e          -> Pr (f l) e
    VRes l vbs e             -> VRes (f l) vbs e
    VExt l e               -> VExt (f l) e

---------------------
--  Pretty printer --
---------------------

instance PrettyAST (Module SrcSpanInfo) where
  ppE (Module srcLocInfo moduleHead importDecl decl) =
        nest 2 $ parens $ ppE "Module" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE moduleHead <> line
    <+> pplist ppE importDecl <> line
    <+> pplist ppE decl
  ppP (Module srcLocInfo moduleHead importDecl decl) =
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
  ppE (Lambda srcLocInfo pat exp) =
        nest 2 $ parens $ ppE "Lambda" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE pat <> line
    <+> ppE exp
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
  -- ppP (NegApp _ exp) = ppP "-" <> parens (ppP exp)
  ppP (If _ e1 e2 e3) = parens $ ppP "If" <+> ppP e1 <+> ppP "then"  <+> ppP e2 <+> ppP "else" <+> ppP e3
  ppP (Lambda _ p e) = parens $ backslash <> ppP p <> dot <> ppP e
  ppP (Pr _ e) = brackets $ ppP e
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
  ppE (PBox srcLocInfo pat) =
        nest 2 $ parens $ ppE "PBox" <> line
    <+> ppE srcLocInfo <> line
    <+> ppE pat
  ppP (PVar _ name) = ppP name
  ppP (PLit _ sign literal) = ppP sign <> ppP literal
  ppP (PWildCard _) = ppP "_"
  ppP (PBox _ pat) = brackets $ ppP pat
