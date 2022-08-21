module PatternSynthesis where

import Control.Monad.State

import Syntax.Env
import Syntax.Type
import Syntax.LambdaVL
import Syntax.Substitution

data PatternEnv' = Env'
  { counter :: Int
  , uEnv :: UEnv   -- 
  , rEnv :: REnv   -- リソース環境
  }
type Env a = State PatternEnv' a

primEnv :: PatternEnv'
primEnv = Env' 0 emptyUEnv emptyREnv

prefixNewTyVar :: String
prefixNewTyVar = "_tyvar_"

getUEnv :: Env UEnv
getUEnv = state $ \env@(Env' _ u _) -> (u, env)

getREnv :: Env REnv
getREnv = state $ \env@(Env' _ _ r) -> (r, env)

setUEnv :: UEnv -> Env ()
setUEnv uenv = state $ \(Env' c _ r) -> ((), Env' c uenv r)

setREnv :: REnv -> Env ()
setREnv renv = state $ \(Env' c u _) -> ((), Env' c u renv)

genNewTyVar :: Kind -> Env ()
genNewTyVar kind = state $ \(Env' c u r) ->
  let strvar' = prefixNewTyVar ++ show c
      counter' = c + 1
      uenv' = insert strvar' kind u
  in return $ Env' counter' uenv' r

patternSynthesis :: Pat l -> Type -> Env (TEnv, SubstMap)
patternSynthesis p ty = case p of
    -- PLit _ sign lit -> 
    -- PWildCard _     ->   
    PVar _ name     ->   
    PBox _ pat      ->
    _ -> error "The argument may be PLit or PWildCard."