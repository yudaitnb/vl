module Language.Absyn (
  module Language.Haskell.Exts.Syntax,
  module Syntax.Common,
  HasName(..),
  HasWhere(..),
  -- PrettyAST(..),
  decomposeDecl,
  getImports, getDepEdge, getDecls, getTopSyms,
) where

import Language.Haskell.Exts.Syntax hiding (Name, QName, ModuleName, Literal)

import Syntax.Common

import Util
    ( exactPrint, line, (<+>), nest, parens, pplist, PrettyAST(..), emptyDoc )
import Language.Haskell.Exts (exactPrint)

getImports :: Module l -> [ModName]
getImports (Module _ _ _ importDecls _) = map getName importDecls
getImports _ = error "getImports is not defined in any syntax other than Module."

getDecls :: Module l -> [Decl l]
getDecls (Module _ _ _ _ ds) = ds
getDecls _                   = error "getDecls is not defined in any syntax other than Module."

getTopSyms :: Module l -> [String]
getTopSyms mod = case mod of
  Module _ _ _ _ decls -> map getName decls
  _                    -> error "getTopSyms is not defined in any syntax other than Module."

getDepEdge :: Module l -> [(ModName, ModName)]
getDepEdge mod@(Module _ mh _ importDecls _) =
  let imports = getImports mod
      modname = maybe (error "This moudle does not have module name.") getName mh
  in map (modname,) imports
getDepEdge _ = error "getDepEdge is not defined in any syntax other than Module."

instance HasName (Exp l) where
  getName (Var _ qName) = getName qName
  getName (Lit _ literal) = getName literal
  getName _ = error "Expressions without Var/Literal do not have a name field." 

instance HasName (Pat l) where
  getName (PVar _ name) = getName name
  getName _             = error "Patterns without PVar do not have a name field."

instance HasName (Decl l) where
  getName decl = case decl of
    PatBind _ p _ _ -> getName p
    FunBind _ ms    -> head $ map getName ms
    _               -> error "Decls without PatBind and FunBind do not have a name field."

instance HasName (Match l) where
  getName m = case m of
    Match _ n _ _ _        -> getName n
    InfixMatch _ _ n _ _ _ -> getName n

instance HasName (ImportDecl l) where
  getName (ImportDecl _ importModule _ _ _ _ _ _) = getName importModule

instance HasName (ModuleHead l) where
  getName (ModuleHead _ moduleName _ _) = getName moduleName

instance HasName (Module l) where
  getName (Module _ mh _ _ _) = maybe (error "") getName mh
  getName _ = error "getName function is not defined for a given expresion."

----------------------

class HasWhere a where
  type RemovedWhere a
  rmWhere :: a -> RemovedWhere a

instance HasWhere (Match l) where
  type RemovedWhere (Match l) = Match l
  rmWhere match@(Match l1 name pats (UnGuardedRhs l2 exp) maybeBinds) =
    let patName = PVar (ann name) name in
    case maybeBinds of
      Nothing -> match
      Just bs ->
        let exp' = Let l1 bs exp
            rhs' = UnGuardedRhs l2 exp'
        in Match l1 name pats rhs' Nothing
  rmWhere _ = error ""

instance HasWhere (Decl l) where
  type RemovedWhere (Decl l) = Decl l
  rmWhere pb@(PatBind l1 pat (UnGuardedRhs l2 exp) maybeBinds) =
    case maybeBinds of
      Nothing -> pb
      Just bs ->
        let exp' = Let l1 bs exp
            rhs' = UnGuardedRhs l2 exp'
        in PatBind l1 pat rhs' maybeBinds
  rmWhere (FunBind l match) = FunBind l (fmap rmWhere match)
  rmWhere _ = error "The rmWhere function is not defined for a given expression."

instance HasWhere (Alt l) where
  type RemovedWhere (Alt l) = Alt l
  rmWhere alt@(Alt l p (UnGuardedRhs l2 exp) maybeBinds) =
    case maybeBinds of
      Nothing -> alt
      Just bs -> Alt l p (UnGuardedRhs l2 (Let l bs exp)) Nothing -- [TODO] Let "l"ではない

decomposeDecl :: [Decl l] -> [Decl l]
decomposeDecl = concatMap decompose
  where
    decompose :: Decl l -> [Decl l]
    decompose (FunBind l [])     = []
    decompose (FunBind l (m:ms)) = FunBind l [m] : decompose (FunBind l ms)
    decompose pb@(PatBind {})    = [pb]
    -- decompose (InfixDecl l (Assoc l) (Maybe Int) [Op l])
    decompose _ = error "Declarations other than FunBind/PatBind exist."

---------------------
--  Pretty printer --
---------------------

instance PrettyAST (Module SrcSpanInfo) where
  ppP m = ppP $ exactPrint m []

instance PrettyAST (ModuleHead SrcSpanInfo) where
  ppP m = ppP $ exactPrint m []

instance PrettyAST (ExportSpecList SrcSpanInfo) where
  ppP m = ppP $ exactPrint m []

instance PrettyAST (ExportSpec SrcSpanInfo) where
  ppP m = ppP $ exactPrint m []

instance PrettyAST (ImportDecl SrcSpanInfo) where
  ppP m = ppP $ exactPrint m []

instance PrettyAST (ImportSpecList SrcSpanInfo) where
  ppP m = ppP $ exactPrint m []

instance PrettyAST (ImportSpec SrcSpanInfo) where
  ppP m = ppP $ exactPrint m []

instance PrettyAST (Namespace SrcSpanInfo)  where
  ppP ns = ppP $ exactPrint ns []

instance PrettyAST (Decl SrcSpanInfo) where
  ppP m = ppP $ exactPrint m []

instance PrettyAST (Pat SrcSpanInfo) where
  ppP m = ppP $ exactPrint m []

instance PrettyAST (Match SrcSpanInfo) where
  ppP m = ppP $ exactPrint m []

instance PrettyAST (Rhs SrcSpanInfo) where
  ppP m = ppP $ exactPrint m []

instance PrettyAST (Binds SrcSpanInfo) where
  ppP m = ppP $ exactPrint m []

instance (PrettyAST l) => PrettyAST (VBinds l) where
  ppP (VBinds srcLocInfo lst) = ppP "VBinds" <+> pplist ppP lst

instance (PrettyAST l) => PrettyAST (VBind l) where
  ppP (VBind _ mn v) = ppP "VBind" <+> ppP mn <+> ppP v

instance (PrettyAST l) => PrettyAST (VersionNumber l) where
  ppP (VersionNumber srcLocInfo major minor patch) = ppP major <> ppP "." <> ppP minor <> ppP "." <> ppP patch

instance (PrettyAST l) => PrettyAST (Sign l) where
  ppP (Signless _) = emptyDoc
  ppP (Negative _) = ppP "-"

instance PrettyAST (Exp SrcSpanInfo) where
  ppP m = ppP $ exactPrint m []

instance PrettyAST (QOp SrcSpanInfo) where
  ppP m = ppP $ exactPrint m []