module Main where

import Control.Monad ( forM_, when )
import Control.Monad.State ( execStateT, execState )
import Data.Map ( (!), empty, elems )

import System.Directory ( createDirectoryIfMissing )
import System.FilePath ( splitFileName, splitExtension )
import System.Environment ( getArgs )

import DependencyGraph
import Compilation.Compile
import Inference.TypeInference (TypedExp(..), aggregateConstraints)
import Syntax.Type (landC)
import qualified Data.List
import Syntax.Version (Version(..))

import Solver
import Util
import Data.List (nub)


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

  let initCompEnv = CompileEnv' (tail sorted) 0 mapParsedAst empty empty empty empty logfile
  env <- execStateT (compile $ head sorted) initCompEnv

  logP "=== Compilation Result ==="
  let result = (! root) $ globalTEnv env
  logP result

  logP "=== Constraints ==="
  let consMain = aggregateConstraints result
      consDepMods = foldl1 landC $ elems $ bundledConstraints env
      cons = consMain `landC` consDepMods
      -- [TODO] namedConstraintの名前が被らないように入れているnubが余計かもしれない
      -- リソース変数が同じでも制約名を区別できるようになればnub不要
  logP $ putDocString $ ppP cons

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

  solve extMods cons >>= \case
    Left (h,r)   -> do
        logP h
        logP $ putDocString $ concatWith (surround $ comma <> space) $ map ppP r
    Right res -> printResult maxWHeader maxWItem res

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

