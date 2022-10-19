module Translation.Desugar where

import qualified Syntax.Desugared as Desugared
import qualified Syntax.Absyn as Absyn
import Syntax.Name
import Syntax.Literal
import Syntax.Label
import Syntax.Version
import qualified Data.Set
import qualified Data.Map

desugarAST :: Absyn.Module l -> Desugared.Module l
desugarAST (Absyn.Module l mh _ imp decls) =
  let
    decls' = map desugar $ Absyn.decomposeDecl decls
  in
    Desugared.Module l mh imp decls'
desugarAST _ = error ""

class Desugaring ast where
  type Desugared ast
  desugar :: ast -> Desugared ast

instance Desugaring (Absyn.Module l) where
  type Desugared (Absyn.Module l) = (Desugared.Module l)
  desugar (Absyn.Module l moduleHead _ importDecl decl) = Desugared.Module l moduleHead importDecl (fmap desugar decl)
  desugar _ = error "The desugaring translation is not defined for a given expression."

instance Desugaring (Absyn.Pat l) where
  type Desugared (Absyn.Pat l) = (Desugared.Pat l)
  desugar (Absyn.PVar l name) =  Desugared.PVar l name
  desugar (Absyn.PLit l sign literal) = Desugared.PLit l sign literal
  -- desugar (Absyn.PWildCard l) = PWildCard l
  desugar _ = error "[Pat@Desugar.hs] The desugaring translation is not defined for a given expression."

-- Multiple top-level functions can be thought of as one big recursive let binding:
-- | f0 x0 = e0
-- | f1 x1 = e1
-- | main = e2
-- ... is equivalent to:
-- | main = let f0 x0 = e0
-- |            f1 x1 = e1
-- |        in  e2

-- Functions are equivalent to lambdas:
-- | f x y z = e
-- ... is equivalent to:
-- | f = \x y z -> e
-- ... which in turn desugars to:
-- | f = \x -> \y -> \z -> e
instance Desugaring (Absyn.Decl l) where
  type Desugared (Absyn.Decl l) = Desugared.Decl l
  desugar fb@(Absyn.FunBind l match) =
    case match of
      []  -> error ""
      [m] ->
        --   desugar (name ps = rhs)
        -- = name = desugar (\ps -> rhs)
        let Absyn.Match l1 name ps (Absyn.UnGuardedRhs l2 exp) _ = Absyn.rmWhere m
            patName = Desugared.PVar (Absyn.ann name) name
            lam' = desugar $ Absyn.Lambda l1 ps exp
        in Desugared.PatBind l1 patName lam'
      m:ms -> error "Before desugaring FunDecl, decompose it."
  desugar pb@(Absyn.PatBind {}) =
    let
      Absyn.PatBind l1 pat (Absyn.UnGuardedRhs l2 exp) Nothing  = Absyn.rmWhere pb
      pat' = desugar pat
      exp' = desugar exp
    in Desugared.PatBind l1 pat' exp'
  desugar _ = error "The desugaring translation is not defined for a given expression."

instance Desugaring (Absyn.Rhs l) where
  type Desugared (Absyn.Rhs l) = Desugared.Exp l
  desugar (Absyn.UnGuardedRhs l1 exp) = desugar exp
  desugar _ = error ""

instance Desugaring (Absyn.Exp l) where
  type Desugared (Absyn.Exp l) = Desugared.Exp l
  -- desugar '(' e ')' =  desugar e
  desugar (Absyn.Paren l e) = desugar e

  -- desugar x -> x
  desugar (Absyn.Var l qName) = Desugared.Var l qName

  -- desugar c -> c
  desugar (Absyn.Lit l literal) = Desugared.Lit l literal

  -- desugar (f ○ x) -> desugar f ○ desugar x
  desugar (Absyn.App l exp1 exp2) = Desugared.App l (desugar exp1) (desugar exp2)

  -- desugar ('-' e) = 0 '-' desugar e
  desugar (Absyn.NegApp l exp) = -- Desugared.NegApp l (desugar exp)
    let varOp = Desugared.Var l (UnQual l (Ident l "-"))
        e1' = Desugared.Lit l (Desugared.Int l 0 "0")
        e2' = desugar exp
    in Desugared.App l (Desugared.App l varOp e1') e2'

  -- desugar ('If' e1 'then' e2 'else' e3) = 'If' desugar e1 'then' desugar e2 'else' desugar e3
  desugar (Absyn.If l exp1 exp2 exp3) = Desugared.If l (desugar exp1) (desugar exp2) (desugar exp3)

  -- ^ desugar (e1 '+' e2) = ('+' (desugar e1)) (desugar e2)
  desugar (Absyn.InfixApp l1 e1 (Absyn.QVarOp l2 qName) e2) =
    let varOp = Desugared.Var l2 qName
        e1' = desugar e1
        e2' = desugar e2
    in Desugared.App l1 (Desugared.App l2 varOp e1') e2'

  -- ^ desugar (\_ -> e)      = error
  -- ^ desugar (\[p] -> e)    = \p -> e
  -- ^ desugar (\(p:ps) -> e) = \p -> desugar (\ps -> e)
  desugar (Absyn.Lambda l pat e) =
    case pat of
      []   -> error ""
      [p]  ->
        let e' = desugar e
            p' = desugar p
        in Desugared.Lambda l p' e'
      p:ps ->
        let lam' = desugar (Absyn.Lambda l ps e)
            pat' = desugar p
        in Desugared.Lambda l pat' lam'

  -- ^ desugar (let [] in exp)          = desugar exp
  -- ^ desugar $ let (x = y):binds in z = (\x -> desugar (let binds in z)) y
  desugar (Absyn.Let l1 binds exp) =
    case binds of
      Absyn.BDecls l2     [] -> desugar exp
      Absyn.BDecls l2 (b:bs) ->
        let zexp' = desugar $ Absyn.Let l1 (Absyn.BDecls l2 bs) exp
            Desugared.PatBind l1 xpat yexp = desugar b
            lam  = Desugared.Lambda l1 xpat zexp'
        in Desugared.App l1 lam yexp
      _                      -> error ""

  -- ^ desugar (version A = x.y.z in exp)          = desugar exp
  desugar (Absyn.VRes l1 vbs exp) = Desugared.VRes l1 (vbsToCs vbs) (desugar exp)
  desugar (Absyn.VExt l1 exp) = Desugared.VExt l1 (desugar exp)

  desugar _ = error ""

vbsToCs :: Absyn.VBinds l -> Labels
vbsToCs (Absyn.VBinds _ vbs) = vbsToCs' vbs
  where
    vbsToCs' :: [Absyn.VBind l] -> Labels
    vbsToCs' vbs = case vbs of
      []     -> mempty
      (Absyn.VBind _ mn v):rst ->
        let mn' = getName mn
            v'  = vnToV v
        in  Data.Set.fromList [Label $ Data.Map.fromList [(mn', v')]] <> vbsToCs' rst