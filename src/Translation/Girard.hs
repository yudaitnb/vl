module Translation.Girard (
  girardFwd
, girardBck
,
) where

import qualified Language.Absyn as AB
import qualified Language.Desugared as DS
import qualified Language.LambdaVL as VL

class GirardFwd ast where
  type TyGirardFwd ast
  girardFwd :: ast -> TyGirardFwd ast

instance GirardFwd (DS.Module l) where
  type TyGirardFwd (DS.Module l) = (VL.Module l)
  girardFwd (DS.Module l moduleHead pragmas importDecl decl) = VL.Module l moduleHead pragmas importDecl (fmap girardFwd decl)

instance GirardFwd (DS.Exp l) where
  type TyGirardFwd (DS.Exp l) = (VL.Exp l)
  girardFwd exp = case exp of
    DS.Var l qName -> VL.Var l qName
    DS.Lit l literal -> VL.Lit l literal
    DS.App l exp1 exp2 -> VL.App l (girardFwd exp1) (VL.Pr (DS.ann exp2) (girardFwd exp2))
    DS.Lambda l pat exp -> 
      let src = DS.ann pat
          pat' = girardFwd pat
          pbox = VL.PBox src pat'
      in VL.Lambda l pbox (girardFwd exp)
    -- DS.Tuple l elms -> VL.Tuple l $ map (\e -> VL.Pr (DS.ann e) (girardFwd e)) elms
    DS.Tuple l elms -> VL.Tuple l $ map girardFwd elms
    -- DS.List l elms -> VL.List l $ map (\e -> VL.Pr (DS.ann e) (girardFwd e)) elms
    DS.List l elms -> VL.List l $ map girardFwd elms
    -- DS.Let l binds exp -> Let l (girardFwd binds) (girardFwd exp)
    DS.Case l e alts -> VL.Case l (VL.Pr (DS.ann e) (girardFwd e)) (map girardFwd alts)
    DS.If l exp1 exp2 exp3 -> VL.If l (girardFwd exp1) (girardFwd exp2) (girardFwd exp3)
    DS.VRes l ls e -> VL.VRes l ls (girardFwd e)
    DS.VExt l e -> VL.VExt l $ VL.Pr (DS.ann e) (girardFwd e)

instance GirardFwd (DS.Pat l) where
  type TyGirardFwd (DS.Pat l) = (VL.Pat l)
  girardFwd p = case p of
    DS.PVar l name ->  VL.PVar l name
    DS.PLit l sign lit -> VL.PLit l sign lit
    DS.PWildCard l -> VL.PWildCard l
    DS.PTuple l pats -> VL.PTuple l (map girardFwd pats)
    DS.PList l pats -> VL.PList l (map girardFwd pats)
    DS.PApp l qn ps -> VL.PApp l qn (map girardFwd ps)
    DS.PInfixApp l p1 qn p2 -> VL.PInfixApp l (girardFwd p1) qn (girardFwd p2)
    -- _ -> error "[Pat@Girard.hs] The girard's (forward) translation is not defined for a given expression."

instance GirardFwd (DS.Alt l) where
  type TyGirardFwd (DS.Alt l) = (VL.Alt l)
  girardFwd (DS.Alt l p e) = VL.Alt l (VL.PBox (DS.ann p) (girardFwd p)) (girardFwd e)

instance GirardFwd (DS.Decl l) where
  type TyGirardFwd (DS.Decl l) = (VL.Decl l)
  -- girardFwd (DS.FunBind l match) = FunBind l (fmap girardFwd match)
  girardFwd (DS.PatBind l pat exp) = VL.PatBind l (girardFwd pat) (girardFwd exp)


class GirardBck ast where
  type TyGirardBck ast
  girardBck :: ast -> TyGirardBck ast

instance GirardBck (VL.Module l) where
  type TyGirardBck (VL.Module l) = (AB.Module l)
  girardBck (VL.Module l moduleHead pragmas importDecl decl) = AB.Module l moduleHead pragmas importDecl (fmap girardBck decl)

instance GirardBck (VL.Exp l) where
  type TyGirardBck (VL.Exp l) = (AB.Exp l)
  girardBck exp = case exp of
    VL.Var l qName -> AB.Var l qName
    VL.Lit l literal -> AB.Lit l literal
    VL.App l exp1 exp2 -> AB.App l (girardBck exp1) (girardBck exp2)
    VL.Lambda l pat exp -> AB.Lambda l [girardBck pat] (girardBck exp)
    VL.Tuple l elms -> AB.Tuple l AB.Boxed $ map girardBck elms
    VL.List l elms  -> AB.List l $ map girardBck elms
    -- VL.Let l binds exp -> AB.Let l (girardBck binds) (girardBck exp)
    VL.Case l exp alts -> AB.Case l (girardBck exp) (map girardBck alts)
    VL.If l exp1 exp2 exp3 -> AB.If l (girardBck exp1) (girardBck exp2) (girardBck exp3)
    VL.Pr l e -> girardBck e
    VL.VRes l ls e -> girardBck e
    VL.VExt l e -> girardBck e

instance GirardBck (VL.Pat l) where
  type TyGirardBck (VL.Pat l) = (AB.Pat l)
  girardBck (VL.PVar l name) =  AB.PVar l name
  girardBck (VL.PLit l sign literal) = AB.PLit l sign literal
  girardBck (VL.PWildCard l) = AB.PWildCard l
  girardBck (VL.PBox l p) = girardBck p
  girardBck (VL.PTuple l ps) = AB.PTuple l AB.Boxed (map girardBck ps)
  girardBck (VL.PList l ps) = AB.PList l (map girardBck ps)
  girardBck (VL.PApp l qn ps) = AB.PList l (map girardBck ps)
  girardBck (VL.PInfixApp l p1 qn p2) = AB.PInfixApp l (girardBck p1) qn (girardBck p2)

instance GirardBck (VL.Decl l) where
  type TyGirardBck (VL.Decl l) = (AB.Decl l)
  girardBck (VL.PatBind l pat exp) = AB.PatBind l (girardBck pat) (AB.UnGuardedRhs l $ girardBck exp) Nothing

instance GirardBck (VL.Alt l) where
  type TyGirardBck (VL.Alt l) = (AB.Alt l)
  girardBck (VL.Alt l p e) = AB.Alt l (girardBck p) (AB.UnGuardedRhs l (girardBck e)) Nothing
  -- [TODO] 位置情報はlではない