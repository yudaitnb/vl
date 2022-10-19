module Translation.Renamer (alphaRename) where

import Data.Map ( Map, insert, empty, findWithDefault, map, lookup, mapKeys, keys, toList, fromList )
import Data.Maybe (fromMaybe)
import Control.Monad.State

import Syntax.Absyn
import qualified Syntax.LambdaVL as VL
import Syntax.Type as T
import Syntax.Name as N
import Syntax.Kind as K
import qualified Syntax.Env as E
import Control.Arrow

alphaRename :: Renamer ast => ast -> (ast, Int)
alphaRename ast =
  let (res, env) = runState (rename ast) primEnv
  in (res, counter env)

class Renamer ast where
  rename :: ast -> Env ast

data Env' = Env'
  { counter :: Int             -- 束縛変数の累積数
  , table :: Map String String -- α-変換の為のHashMapテーブル, スコープ生成の度に変わり得る
  -- , record :: Map String String -- 過去の型変数の対応表。モジュールを跨がない限り初期化しない
  }
type Env a = State Env' a

primEnv :: Env'
primEnv = Env' 0 empty
  -- empty

prefixNewVar :: String
prefixNewVar = "x"

prefixNewTyVar :: String
prefixNewTyVar = "a"

addC ::Env ()
addC = modify $ \env -> env { counter = 1 + counter env}

setTable :: Map String String -> Env ()
setTable table = modify $ \env -> env { table = table }

initializeTable :: Env ()
initializeTable = modify $ \env -> env { table = empty }

-- initializeRecord :: Env ()
-- initializeRecord = modify $ \env -> env { record = empty }

genNewVarPats :: [Pat l] -> Env [Pat l]
genNewVarPats = mapM genNewVarPat
  where
    genNewVarPat :: Pat l -> Env (Pat l)
    genNewVarPat p = do
      c <- gets counter
      t <- gets table
      case p of
        (PVar l1 (N.Ident l2 str)) -> do
          let strvar' = prefixNewVar ++ show c
              pat' = PVar l1 (N.Ident l2 strvar')
          addC
          setTable $ insert (getName p) strvar' t
          return pat'
        p -> return p

genNewTyVar :: String -> Env T.Type
genNewTyVar str = do
  c <- gets counter
  t <- gets table
  let name = prefixNewTyVar ++ show c
      newvar = T.TyVar (T.Ident name)
  addC
  setTable $ insert str name t
  return newvar

-- 項のα変換
instance Renamer (Exp l) where
  rename exp = do
    cur_table <- gets table
    case exp of
      var@(Var l1 (N.UnQual l2 (N.Ident l3 str)))
                    -> let resstr = findWithDefault str str cur_table in
                       return $ Var l1 (N.UnQual l2 (N.Ident l3 resstr))
      var@(Var _ _) -> return var
      lit@(Lit _ _) -> return lit
      App l e1 e2   -> do e1' <- rename e1
                          e2' <- rename e2
                          return $ App l e1' e2'
      NegApp l e    -> do e' <- rename e
                          return $ NegApp l e'
      If l e1 e2 e3 -> do e1' <- rename e1
                          e2' <- rename e2
                          e3' <- rename e3
                          return $ If l e1' e2' e3'
      InfixApp l e1 op e2 -> do e1' <- rename e1
                                op' <- rename op
                                e2' <- rename e2
                                return $ InfixApp l e1' op' e2'
      -- If even one pattern that binds the same name exists, terminate the rename function,
      Lambda l ps e -> do oldTable <- gets table   -- 旧テーブルを保存
                          ps' <- genNewVarPats ps  -- 束縛変数から新しいテーブルを環境に登録/新しい束縛変数リストを得る
                          e' <- rename e           -- 更新された環境でrename
                          setTable oldTable        -- 旧テーブルに戻す
                          return $ Lambda l ps' e' -- rename済み式と新しい束縛変数リストを用いてLambdaのrename済み式を構成
      _ -> error "[Exp - rename@Rename.hs] The alpha-renameExp function is not defined for a given expression."

instance Renamer (Module l) where
  rename (Module l moduleHead modulePragma importDecl decl) = do decl' <- mapM rename decl
                                                                 return $ Module l moduleHead modulePragma importDecl decl'
  rename _ = error "[Module - rename@Rename.hs] The alpha-renameExp function is not defined for a given expression."

instance Renamer (Decl l) where
  rename (FunBind l match) = do ms' <- mapM rename match
                                return $ FunBind l ms'
  rename (PatBind l pat rhs maybeBinds) = do oldTable <- gets table
                                             rhs' <- rename rhs
                                             setTable oldTable
                                             return $ PatBind l pat rhs' maybeBinds -- TODO:bindsも同様にスコープを作る。
  rename _ = error "[Decl - rename@Rename.hs] The alpha-renameExp function is not defined for a given expression."

instance Renamer (Match l) where
  rename (Match l name ps rhs maybeBinds) = do oldTable <- gets table
                                               ps' <- genNewVarPats ps
                                               rhs' <- rename rhs
                                               setTable oldTable
                                               return $ Match l name ps' rhs' maybeBinds -- TODO:bindsも同様にスコープを作る。
  rename ms@(InfixMatch l _ _ _ _ _) = error "[Match - rename@Rename.hs] The alpha-renameExp function is not defined for a given expression."

instance Renamer (Rhs l) where
  rename (UnGuardedRhs l e) = do e' <- rename e
                                 return $ UnGuardedRhs l e'
  rename rhs@(GuardedRhss l guardedRhs) = error "[Rhs - rename@Rename.hs] The alpha-renameExp function is not defined for a given expression."

instance Renamer (QOp l) where
  rename (QVarOp l qName) = return $ QVarOp l qName
  rename (QConOp l qName) = return $ QConOp l qName

-- ------------------------

instance Renamer T.Type where
  rename ty = case ty of
    -- ^ Types
    (T.TyCon qName) -> return ty
    (T.TyFun t1 t2) -> T.TyFun <$> rename t1 <*> rename t2
    (T.TyVar name)  -> do
      cur_table <- gets table
      let T.Ident n = name
      case Data.Map.lookup n cur_table of
        Nothing -> do
          genNewTyVar n
        Just res -> return $ T.TyVar (T.Ident res)
    (T.TyBox c ty)  -> T.TyBox <$> rename c <*> rename ty
    -- ^ Coeffects
    T.TyBottom     -> return ty
    (T.TyLabels p) -> return ty
    (T.CAdd c1 c2) -> T.CAdd <$> rename c1 <*> rename c2
    (T.CMul c1 c2) -> T.CMul <$> rename c1 <*> rename c2
    -- ^ Constraints
    (T.CSubset c1 c2) -> T.CSubset <$> rename c1 <*> rename c2

-- ^ 個々のTEnvの型変数は変数空間が独立な事を仮定
-- ^ Γ = {f : T, g : S}
-- ^ isnull $ freeTyVars T `intersect` freeTyVars S
instance Renamer E.TEnv where
  rename = mapM rename

instance Renamer E.EnvType where
  rename envty = case envty of
    E.NType tag ty -> E.NType tag <$> rename ty
    E.GrType tag ty c -> E.GrType tag <$> rename ty <*> rename c

instance Renamer E.UEnv where
  rename uenv = do
    uenv' <- mapM rename $ Data.Map.toList uenv
    return $ Data.Map.fromList uenv'

instance Renamer (String, K.Kind) where
  rename (s, k) = do
    s' <- rename s
    return (s', k)

instance Renamer String where
  rename s = do
    cur_table <- gets table
    case Data.Map.lookup s cur_table of
      Nothing  -> error $ "Renamer cannot find string: " ++ show s
      Just res -> return res

-- instance Renamer K.Kind where
--   rename = return

instance Renamer T.Constraints where
  rename = mapM rename

-- 暗黙に各要素はModuleに対応しているため、モジュールを跨ぐ再帰のタイミングでテーブルを初期化
instance Renamer [(E.TEnv, E.UEnv, Constraints)] where
  rename [] = return []
  rename ((tenv, uenv, con) : lst) = do
    -- 同じテーブルでtenv, uenv, conをrename
    tenv' <- rename tenv
    uenv' <- rename uenv
    con' <- rename con
    -- カウントだけそのまま、テーブルは初期化して残りをrename
    initializeTable
    lst' <- rename lst
    return $ (tenv', uenv', con') : lst'