module Renamer (alphaRename) where

import Syntax.Absyn
import Data.Maybe (fromMaybe)
import Control.Monad.State
import Data.Map

alphaRename :: AlphaRename ast => ast -> ast
alphaRename ast = 
  evalState (rename ast) primEnv

class AlphaRename ast where
  rename :: ast -> Env ast

data Env' = Env'
  { counter :: Int             -- 束縛変数の累積数
  , table :: Map String String -- α-変換の為のHashMapテーブル, スコープ生成の度に変わり得る
  }
type Env a = State Env' a

primEnv :: Env'
primEnv = Env' 0 empty

prefixNewVar :: String
prefixNewVar = "_var_"

getTable :: Env (Map String String)
getTable = state $ \(Env' c t) -> (t, Env' c t)

setTable :: Map String String -> Env ()
setTable table = state $ \(Env' c _) -> ((), Env' c table)

genNewVarPats :: [Pat l] -> Env [Pat l]
genNewVarPats = mapM genNewVar
  where
    genNewVar :: Pat l -> Env (Pat l)
    genNewVar p@(PVar l1 (Ident l2 str)) = state $ \(Env' c t) ->
      let strvar' = prefixNewVar ++ show c
          pat' = PVar l1 (Ident l2 strvar')
          counter' = c + 1
          table' = insert (getName p) strvar' t
      in (pat', Env' counter' table')
    genNewVar p = state $ \e -> (p, e)

instance AlphaRename (Exp l) where
  rename exp = do
    cur_table <- gets table
    case exp of
      var@(Var l1 (UnQual l2 (Ident l3 str)))
                    -> let resstr = findWithDefault str str cur_table in
                       return $ Var l1 (UnQual l2 (Ident l3 resstr))
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
      Lambda l ps e -> do oldTable <- getTable     -- 旧テーブルを保存
                          ps' <- genNewVarPats ps  -- 束縛変数から新しいテーブルを環境に登録/新しい束縛変数リストを得る
                          e' <- rename e           -- 更新された環境でrename
                          setTable oldTable        -- 旧テーブルに戻す
                          return $ Lambda l ps' e' -- rename済み式と新しい束縛変数リストを用いてLambdaのrename済み式を構成
      _              -> error "[Exp - rename@Rename.hs] The alpha-renameExp function is not defined for a given expression."

instance AlphaRename (Module l) where
  rename (Module l moduleHead modulePragma importDecl decl) = do decl' <- mapM rename decl
                                                                 return $ Module l moduleHead modulePragma importDecl decl'
  rename _ = error "[Module - rename@Rename.hs] The alpha-renameExp function is not defined for a given expression."

instance AlphaRename (Decl l) where
  rename (FunBind l match) = do ms' <- mapM rename match
                                return $ FunBind l ms'
  rename (PatBind l pat rhs maybeBinds) = do oldTable <- getTable
                                             rhs' <- rename rhs
                                             setTable oldTable
                                             return $ PatBind l pat rhs' maybeBinds -- TODO:bindsも同様にスコープを作る。
  rename _ = error "[Decl - rename@Rename.hs] The alpha-renameExp function is not defined for a given expression."

instance AlphaRename (Match l) where
  rename (Match l name ps rhs maybeBinds) = do oldTable <- getTable
                                               ps' <- genNewVarPats ps
                                               rhs' <- rename rhs
                                               setTable oldTable
                                               return $ Match l name ps' rhs' maybeBinds -- TODO:bindsも同様にスコープを作る。
  rename ms@(InfixMatch l _ _ _ _ _) = error "[Match - rename@Rename.hs] The alpha-renameExp function is not defined for a given expression."

instance AlphaRename (Rhs l) where
  rename (UnGuardedRhs l e) = do e' <- rename e
                                 return $ UnGuardedRhs l e'
  rename rhs@(GuardedRhss l guardedRhs) = error "[Rhs - rename@Rename.hs] The alpha-renameExp function is not defined for a given expression."

instance AlphaRename (QOp l) where
  rename (QVarOp l qName) = return $ QVarOp l qName
  rename (QConOp l qName) = return $ QConOp l qName