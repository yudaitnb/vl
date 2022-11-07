module Translation.Girard (
  girardFwd, girardBck
) where

import qualified Syntax.Absyn as AB
import qualified Syntax.Desugared as Desugared
import qualified Syntax.LambdaVL as VL
import Syntax.Literal
-- import Syntax.Type

class GirardFwd ast where
  type TyGirardFwd ast
  girardFwd :: ast -> TyGirardFwd ast

instance GirardFwd (Desugared.Module l) where
  type TyGirardFwd (Desugared.Module l) = (VL.Module l)
  girardFwd (Desugared.Module l moduleHead pragmas importDecl decl) = VL.Module l moduleHead pragmas importDecl (fmap girardFwd decl)

instance GirardFwd (Desugared.Exp l) where
  type TyGirardFwd (Desugared.Exp l) = (VL.Exp l)
  girardFwd (Desugared.Var l qName) = VL.Var l qName
  girardFwd (Desugared.Lit l literal) = VL.Lit l literal
  girardFwd (Desugared.App l exp1 exp2) = VL.App l (girardFwd exp1) (VL.Pr (Desugared.ann exp2) (girardFwd exp2))
  girardFwd (Desugared.Lambda l pat exp) = 
    let src = Desugared.ann pat
        pat' = girardFwd pat
        pbox = VL.PBox src pat'
    in VL.Lambda l pbox (girardFwd exp)
  -- girardFwd (Desugared.Let l binds exp) = Let l (girardFwd binds) (girardFwd exp)
  girardFwd (Desugared.If l exp1 exp2 exp3) = VL.If l (girardFwd exp1) (girardFwd exp2) (girardFwd exp3)
  girardFwd (Desugared.VRes l ls e) =
    VL.VRes l ls (girardFwd e)
    -- VL.VRes l ls $ VL.Pr (Desugared.ann e) (girardFwd e)
  girardFwd (Desugared.VExt l e) = VL.VExt l $ VL.Pr (Desugared.ann e) (girardFwd e)

instance GirardFwd (Desugared.Pat l) where
  type TyGirardFwd (Desugared.Pat l) = (VL.Pat l)
  girardFwd (Desugared.PVar l name) =  VL.PVar l name
  girardFwd (Desugared.PLit l sign literal) = VL.PLit l sign literal
  -- girardFwd (Desugared.PWildCard l) = PWildCard l
  girardFwd _ = error "[Pat@Girard.hs] The girard's (forward) translation is not defined for a given expression."

instance GirardFwd (Desugared.Decl l) where
  type TyGirardFwd (Desugared.Decl l) = (VL.Decl l)
  -- girardFwd (Desugared.FunBind l match) = FunBind l (fmap girardFwd match)
  girardFwd (Desugared.PatBind l pat exp) = VL.PatBind l (girardFwd pat) (girardFwd exp)


class GirardBck ast where
  type TyGirardBck ast
  girardBck :: ast -> TyGirardBck ast

instance GirardBck (VL.Module l) where
  type TyGirardBck (VL.Module l) = (AB.Module l)
  girardBck (VL.Module l moduleHead pragmas importDecl decl) = AB.Module l moduleHead pragmas importDecl (fmap girardBck decl)

instance GirardBck (VL.Exp l) where
  type TyGirardBck (VL.Exp l) = (AB.Exp l)
  girardBck (VL.Var l qName) = AB.Var l qName
  girardBck (VL.Lit l literal) = AB.Lit l literal
  girardBck (VL.App l exp1 exp2) = AB.App l (girardBck exp1) (girardBck exp2)
  girardBck (VL.Lambda l pat exp) = AB.Lambda l [girardBck pat] (girardBck exp)
  -- girardBck (VL.Let l binds exp) = Let l (girardBck binds) (girardBck exp)
  girardBck (VL.If l exp1 exp2 exp3) = AB.If l (girardBck exp1) (girardBck exp2) (girardBck exp3)
  girardBck (VL.Pr l e) = girardBck e
  girardBck (VL.VRes l ls e) = girardBck e
  girardBck (VL.VExt l e) = girardBck e

instance GirardBck (VL.Pat l) where
  type TyGirardBck (VL.Pat l) = (AB.Pat l)
  girardBck (VL.PVar l name) =  AB.PVar l name
  girardBck (VL.PLit l sign literal) = AB.PLit l sign literal
  girardBck (VL.PWildCard l) = AB.PWildCard l
  girardBck (VL.PBox l p) = girardBck p

instance GirardBck (VL.Decl l) where
  type TyGirardBck (VL.Decl l) = (AB.Decl l)
  girardBck (VL.PatBind l pat exp) = AB.PatBind l (girardBck pat) (AB.UnGuardedRhs l $ girardBck exp) Nothing
