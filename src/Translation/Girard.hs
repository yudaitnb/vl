module Translation.Girard (
  girardFwd
) where

import qualified Syntax.Desugared as Desugared
import qualified Syntax.LambdaVL as VL
import Syntax.Literal
-- import Syntax.Type

class GirardFwd ast where
  type Girard ast
  girardFwd :: ast -> Girard ast

instance GirardFwd (Desugared.Module l) where
  type Girard (Desugared.Module l) = (VL.Module l)
  girardFwd (Desugared.Module l moduleHead importDecl decl) = VL.Module l moduleHead importDecl (fmap girardFwd decl)

instance GirardFwd (Desugared.Exp l) where
  type Girard (Desugared.Exp l) = (VL.Exp l)
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
  type Girard (Desugared.Pat l) = (VL.Pat l)
  girardFwd (Desugared.PVar l name) =  VL.PVar l name
  girardFwd (Desugared.PLit l sign literal) = VL.PLit l sign literal
  -- girardFwd (Desugared.PWildCard l) = PWildCard l
  girardFwd _ = error "[Pat@Girard.hs] The girard's (forward) translation is not defined for a given expression."

instance GirardFwd (Desugared.Decl l) where
  type Girard (Desugared.Decl l) = (VL.Decl l)
  -- girardFwd (Desugared.FunBind l match) = FunBind l (fmap girardFwd match)
  girardFwd (Desugared.PatBind l pat exp) = VL.PatBind l (girardFwd pat) (girardFwd exp)

  