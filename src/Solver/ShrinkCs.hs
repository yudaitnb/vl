module Solver.ShrinkCs where

import Prelude

-- import Data.Map
import Syntax.Type
import Syntax.Common hiding (Name(..))
import Data.Map (Map, empty, fromList, toList, delete, insert, insertWith, size, mapWithKey)
import Control.Monad.State
import Util

data Env' = Env'
  { keys :: [VarKey]
  , flag :: Bool
  , loop :: Int
  , orig :: Constraints
  }
  deriving (Show)
type Env a = State Env' a

setFlag :: Env ()
setFlag = modify $ \env -> env { flag = True }

resetFlag :: Env ()
resetFlag = modify $ \env -> env { flag = False }

addKey :: VarKey -> Env ()
addKey k = modify $ \env -> env { keys = k : keys env }

addKeys :: [VarKey] -> Env ()
addKeys ks = modify $ \env -> env { keys = ks ++ keys env }

addLoop :: Env ()
addLoop = modify $ \env -> env { loop = 1 + loop env }

minimizeCs :: Constraints -> FilePath -> IO Constraints
minimizeCs cs logFilePath = do
  logP "\n=== Constraints (normalized) ==="
  let initState = Env' [] False 0 cs
      env = execState (accVars cs) initState
      ks = keys env
      l = loop env
      sizePre = sizeCs cs
  let res = minimizeCs' cs ks
      sizePro = sizeCs cs
  logPD $ ppP "Constraint size (pre-shrinking) :" <+> ppP sizePre
  let persec = 100 * fromIntegral sizePro / fromIntegral sizePre :: Float
  logPD $ ppP "Constraint size (pro-shrinking) :" <+> ppP sizePro <+> parens (ppP persec <+> ppP "%")
  logPD $ ppP cs

  return res
  where
    logP :: PrettyAST a => a -> IO ()
    logP = logPpLn ppP logFilePath
    logPD :: Doc ann -> IO ()
    logPD = logPpLnDoc logFilePath
    minimizeCs' :: Constraints -> [VarKey] -> Constraints
    minimizeCs' cs keys = case cs of
      CTop -> CTop
      CSubset (TyVar _) (TyLabels _) -> cs
      CSubset (TyVar _) (TyVar n2) -> if UQVar (getName n2) `elem` keys then cs else CTop
      CAnd c1 CTop  -> c1
      CAnd CTop c2  -> c2
      CAnd c1 c2    -> let c1' = minimizeCs' c1 keys
                           c2' = minimizeCs' c2 keys in
                       case (c1', c2') of
                        (CTop, _) -> c2'
                        (_, CTop) -> c1'
                        _         -> CAnd c1' c2'
      COr CTop _    -> CTop
      COr _ CTop    -> CTop
      COr c1 c2     -> let c1' = minimizeCs' c1 keys
                           c2' = minimizeCs' c2 keys in
                       case (c1', c2') of
                        (CTop, _) -> CTop
                        (_, CTop) -> CTop
                        _         -> COr c1' c2'

    accVars :: Constraints -> Env ()
    accVars cs = do
      resetFlag
      ks <- gets keys
      if null ks
        then accVarsLabelDep cs
        else accVarsDepLabelDep cs
      addLoop
      gets flag >>= \f -> when f $ accVars cs

    -- Subsequent loop for variable dependencies
    accVarsDepLabelDep :: Constraints -> Env ()
    accVarsDepLabelDep cs = case cs of
      CTop -> return ()
      CSubset c1 (TyLabels _) -> return ()
      CSubset (TyVar n1) (TyVar n2) -> do
        let k1 = UQVar (getName n1)
            k2 = UQVar (getName n2)
        ks <- gets keys
        when (k1 `notElem` ks && k2 `elem` ks) $ setFlag >> addKey k1
        when (k1 `elem` ks && k2 `notElem` ks) $ setFlag >> addKey k2
      CAnd c1 c2 -> forM_ [c1,c2] accVarsDepLabelDep
      COr c1 c2  -> forM_ [c1,c2] accVarsDepLabelDep

    -- Initial loop for label dependencies
    accVarsLabelDep :: Constraints -> Env ()
    accVarsLabelDep cs = case cs of
      CTop -> return ()
      CSubset c1 (TyLabels _) -> setFlag >> addKey (UQVar (getName c1))
      CSubset c1 c2 -> return ()
      CAnd c1 c2    -> forM_ [c1,c2] accVarsLabelDep
      COr c1 c2     -> forM_ [c1,c2] accVarsLabelDep
