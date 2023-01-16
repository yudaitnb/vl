module Translation.Desugar where

import qualified Language.Desugared as DS
import qualified Language.Absyn as AB
import Syntax.Common
import qualified Data.Map

desugarAST :: Show l => AB.Module l -> DS.Module l
desugarAST (AB.Module l mh pragmas imp decls) =
  let
    decls' = map desugar $ AB.decomposeDecl decls
  in
    DS.Module l mh pragmas imp decls'
desugarAST _ = error ""

class Desugaring ast where
  type Desugared ast
  desugar :: ast -> Desugared ast

instance Show l => Desugaring (AB.Module l) where
  type Desugared (AB.Module l) = (DS.Module l)
  desugar (AB.Module l moduleHead pragmas importDecl decl) = DS.Module l moduleHead pragmas importDecl (fmap desugar decl)
  desugar _ = error "The desugaring translation is not defined for a given expression."

instance Show l => Desugaring (AB.Pat l) where
  type Desugared (AB.Pat l) = (DS.Pat l)
  desugar p = case p of
    AB.PWildCard l          -> DS.PWildCard l
    AB.PVar l name          -> DS.PVar l name
    AB.PLit l sign lit      -> DS.PLit l sign lit
    AB.PTuple l _ ps        -> DS.PTuple l (map desugar ps)
    AB.PList l ps           -> DS.PList l (map desugar ps)
    AB.PApp l qn ps         -> DS.PApp l qn (map desugar ps)
    AB.PInfixApp l p1 qn p2 -> DS.PInfixApp l (desugar p1) qn (desugar p2)
    _ -> error $ "\n[Pat@Desugar.hs] The desugaring translation is not defined for a given expression.\n" ++ show p

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
instance Show l => Desugaring (AB.Decl l) where
  type Desugared (AB.Decl l) = DS.Decl l
  desugar fb@(AB.FunBind l match) =
    case match of
      []  -> error ""
      [m] ->
        --   desugar (name ps = rhs)
        -- = name = desugar (\ps -> rhs)
        let AB.Match l1 name ps (AB.UnGuardedRhs l2 exp) _ = AB.rmWhere m
            patName = DS.PVar (AB.ann name) name
            lam' = desugar $ AB.Lambda l1 ps exp
        in DS.PatBind l1 patName lam'
      m:ms -> error "Before desugaring FunDecl, decompose it."
  desugar pb@(AB.PatBind {}) =
    let
      AB.PatBind l1 pat (AB.UnGuardedRhs l2 exp) Nothing  = AB.rmWhere pb
      pat' = desugar pat
      exp' = desugar exp
    in DS.PatBind l1 pat' exp'
  desugar _ = error "The desugaring translation is not defined for a given expression."

instance Show l => Desugaring (AB.Rhs l) where
  type Desugared (AB.Rhs l) = DS.Exp l
  desugar (AB.UnGuardedRhs l1 exp) = desugar exp
  desugar _ = error ""

instance Show l => Desugaring (AB.Alt l) where
  type Desugared (AB.Alt l) = DS.Alt l
  desugar alt =
    let AB.Alt l p rhs Nothing = AB.rmWhere alt 
        p' = desugar p
        exp' = desugar rhs
    in DS.Alt l p' exp'

instance Show l => Desugaring (AB.Exp l) where
  type Desugared (AB.Exp l) = DS.Exp l
  desugar exp = case exp of
    -- desugar '(' e ')' =  desugar e
    AB.Paren l e -> desugar e

    -- desugar x -> x
    AB.Var l qName -> DS.Var l qName

    -- desugar c -> c
    AB.Lit l literal -> DS.Lit l literal

    -- desugar (f ○ x) -> desugar f ○ desugar x
    AB.App l exp1 exp2 -> DS.App l (desugar exp1) (desugar exp2)

    -- desugar ('-' e) -> 0 '-' desugar e
    AB.NegApp l exp -> -- DS.NegApp l (desugar exp)
      let varOp = DS.Var l (UnQual l (Ident l "-"))
          e1' = DS.Lit l (DS.Int l 0 "0")
          e2' = desugar exp
      in DS.App l (DS.App l varOp e1') e2'

    -- desugar ('If' e1 'then' e2 'else' e3) = 'If' desugar e1 'then' desugar e2 'else' desugar e3
    AB.If l exp1 exp2 exp3 -> DS.If l (desugar exp1) (desugar exp2) (desugar exp3)

    -- ^ desugar (e1 '+' e2) = ('+' (desugar e1)) (desugar e2)
    AB.InfixApp l1 e1 (AB.QVarOp l2 qName) e2 ->
      let varOp = DS.Var l2 qName
          e1' = desugar e1
          e2' = desugar e2
      in DS.App l1 (DS.App l2 varOp e1') e2'

    -- ^ desugar (e1 ':' e2) = (':' (desugar e1)) (desugar e2)
    AB.InfixApp l1 e1 (AB.QConOp l2 qName) e2 ->
      let conOp = DS.Con l2 qName
          e1' = desugar e1
          e2' = desugar e2
      in DS.App l1 (DS.App l2 conOp e1') e2'

    -- ^ desugar (\_ -> e)      = error
    -- ^ desugar (\[p] -> e)    = \p -> e
    -- ^ desugar (\(p:ps) -> e) = \p -> desugar (\ps -> e)
    AB.Lambda l pat e ->
      case pat of
        []   -> error ""
        [p]  ->
          let e' = desugar e
              p' = desugar p
          in DS.Lambda l p' e'
        p:ps ->
          let lam' = desugar (AB.Lambda l ps e)
              pat' = desugar p
          in DS.Lambda l pat' lam'

    -- ^ desugar (let [] in exp)          = desugar exp
    -- ^ desugar $ let (x = y):binds in z = (\x -> desugar (let binds in z)) y
    AB.Let l1 binds exp ->
      case binds of
        AB.BDecls l2     [] -> desugar exp
        AB.BDecls l2 (b:bs) ->
          let zexp' = desugar $ AB.Let l1 (AB.BDecls l2 bs) exp
              DS.PatBind l1 xpat yexp = desugar b
              lam  = DS.Lambda l1 xpat zexp'
          in DS.App l1 lam yexp
        _                      -> error $ "\nGiven exp is no supported.\n  binds : " ++ show binds

    -- ^ desugar (version A = x.y.z in exp)          = desugar exp
    AB.VRes l1 vbs exp -> DS.VRes l1 (vbsToCs vbs) (desugar exp)
    AB.VExt l1 exp -> DS.VExt l1 (desugar exp)

    -- ^ 
    AB.Case l e as -> 
      let e' = desugar e 
          as' = map desugar as
      in DS.Case l e' as'

    -- ^ 
    AB.Tuple l boxed elems -> DS.Tuple l $ map desugar elems
    AB.List l elems -> DS.List l $ map desugar elems

    _              -> error $ "\nGiven exp is no supported.\n  exp : " ++ show exp

vbsToCs :: AB.VBinds l -> Label
vbsToCs (AB.VBinds _ vbs) = vbsToCs' vbs
  where
    vbsToCs' :: [AB.VBind l] -> Label
    vbsToCs' vbs = case vbs of
      []     -> mempty
      (AB.VBind _ mn v):rst ->
        let mn' = getName mn
            v'  = vnToV v
        in  Data.Map.fromList [(mn', [v'])] <> vbsToCs' rst