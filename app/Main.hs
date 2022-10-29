module Main where

import Control.Monad ( forM_, when )
import Control.Monad.State ( execStateT, execState )

import Data.List (nub)
import Data.Map ( (!), empty, elems, mapWithKey )

import System.Directory ( createDirectoryIfMissing )
import System.FilePath ( splitFileName, splitExtension )
import System.Environment ( getArgs )

import DependencyGraph
import Compilation.Compile
import Inference.TypeInference (TypedExp(..), aggregateConstraints)
import Syntax.Type (landC, Constraints(..))
import qualified Data.List
import Syntax.Version (Version(..))

import Solver
import SolverGT
import Util


-- import qualified Language.Haskell.Interpreter as I

main :: IO ()
main = do
  let rootDirPath = "./examples/"
      logDirPath = "./log/"
      extension = ".hs"

  -- Get arguments from stdin
  -- stack run "[ROOT].hs"
  [filename] <- getArgs
  let dir = rootDirPath
      (fn, ext) = splitExtension filename

  -- Logger utility
  let logfile = logDirPath ++ fn ++ ".log"
      logE :: PrettyAST a => a -> IO ()
      logE = logPpLn ppE logfile
      logP :: PrettyAST a => a -> IO ()
      logP = logPpLn ppP logfile

  -- Create log directory 
  createDirectoryIfMissing False logDirPath
  removeFileIfExists logfile

  -- Parse dependent modules and create dependency graph
  (root, sorted, mapParsedAst) <- getDependencyGraph fn dir ext
  let extMods = cvtExtMods sorted
  logP "=== Compilation order ==="
  logP sorted

  let initCompEnv = CompileEnv' (tail sorted) 0 Data.Map.empty mapParsedAst empty CTop empty empty empty empty logfile
  env <- execStateT (compile $ head sorted) initCompEnv

  logP "=== Constraints ==="
  let cons = globalConstraints env
  logP $ putDocString $ ppP cons <> line

  logP "=== Solver result ==="
  let fvCons = freeVars cons
      maxWHeader = maximum $ Data.List.map Data.List.length fvCons
      maxWItem = 7 -- [TODO]
      printResult :: Int -> Int -> [(String, [(String, Version)])] -> IO ()
      printResult maxWHeader maxWItem m = do
        forM_ m $ \(v, res) -> do
          let header = fill maxWHeader (ppP v) <+> ppP ":"
              maintxt = concatWith (surround $ comma <> space) $ Data.List.map (\(mn, v) -> fill maxWItem $ ppP mn <> colon <> ppP v) res
              doc = header <+> maintxt
          logP $ putDocString doc

  -- logP "=== SolverGT ==="
  SolverGT.solve extMods cons >>= \case
    Left (h,r)   -> do
        logP h
        logP $ putDocString $ concatWith (surround $ comma <> space) $ map ppP r
    Right res ->
      logP $ putDocString $ concatWith (surround line) $ mapWithKey (\vn l -> ppP vn <+> colon <+> ppP l) res
  -- logP "=== Solver ==="
  -- Solver.solve extMods cons >>= \case
  --   Left (h,r)   -> do
  --       logP h
  --       logP $ putDocString $ concatWith (surround $ comma <> space) $ map ppP r
  --   Right res ->
  --     printResult maxWHeader maxWItem res

  -- putStrLn "=== Standard output ==="
  -- res <- H.runInterpreter $ interp tmpfn func
  -- putStrLn "=== Result value ( expects () ) ==="
  -- case res of
  --   Right res -> print res
  --   Left err  -> throw err

-- interp :: String -> String -> H.InterpreterT IO ()
-- interp path func = do
--   initInterpEnvByFile path
--   H.runStmt func

-- initInterpEnvByFile :: String -> H.Interpreter ()
-- initInterpEnvByFile path = do
--   let mod = dropExtension $ takeFileName path
--   H.reset
--   -- unsafeSetGhcOption ("-i" ++ intercalate ":" confs)
--   -- H.set [H.languageExtensions H.:= [H.ImplicitParams]]
--   -- H.set [H.installedModulesInScope H.:= True]
--   H.loadModules [path]
--   H.setTopLevelModules [mod]
--   H.setImportsQ [("Prelude", Nothing)]

