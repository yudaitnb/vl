module Main where

import Prelude hiding (log)
import System.Directory ( createDirectoryIfMissing, doesPathExist )
import System.FilePath ( splitFileName, splitExtension )
import System.Environment ( getArgs )

import Control.Monad ( forM_, when, unless )
import Control.Monad.State ( execStateT, execState )

import Data.Map ( Map, empty, mapWithKey, toList )
import Data.Maybe (fromMaybe, isJust)
import Data.Either (fromRight)

import Parser ( cvtExtMods, parseDependencyGraph, VLMod(..) )
import Compile (compile, CompileEnv'(..))
import Inference.TypeInference (TypedExp(..), aggregateConstraints)
import Config ( decodeConfig )
import Translation.Extraction ( extract )

import Syntax.Type (landC, Constraints(..), Type, HasName (getName), sizeCs)
import Syntax.Common (Version(..), Label, ParsedAST(..))

import Solver.Solver ( solve )

import Language.Haskell.Exts.Pretty

import Evaluation
import Util

import Solver.ShrinkCs ( minimizeCs )

-- import qualified Language.Haskell.Interpreter as I

main :: IO ()
main = do
  -- Configuring as vlconfig.json
  (rootDirPath, validExt) <- decodeConfig

  -- Get a filename of a root module from stdin, e.g. `stack run "[ROOT_VL_MODULE].hs"`
  [fnRoot] <- getArgs
  let (fnMain, ext) = splitExtension fnRoot
      pathMain = rootDirPath ++ fnMain ++ ext
  when (ext /= validExt) $ error "Illegal VL module extension."
  doesPathExist pathMain >>= \b -> unless b $ error $ show pathMain ++ " does not exists."

  -- Logger utility
  let logFilePath = rootDirPath ++ fnMain ++ ".log"
  removeFileIfExists logFilePath

  -- [Phase] Parsing along with creating dependency graph
  (timeParsing, (root, sortedVLMods, mapParsedAst)) <- timeItT $ parseDependencyGraph fnMain rootDirPath ext logFilePath

  -- [Phase] Compilation
  (timeCompiling, env) <- timeItT $ compile sortedVLMods mapParsedAst logFilePath
  let cs = globalConstraints env

  -- [Phase] Constraints Minimization
  (timeMinimizeCs, cs') <- timeItT $ minimizeCs cs logFilePath

  -- [Phase] Constaints Resolution
  (timeConstraintResolution, solResMap) <- timeItT $ solve (cvtExtMods sortedVLMods) logFilePath cs'

  -- [Phase] Extraction
  (timeExtraction, hsCodeBundled) <- timeItT $ extract env logFilePath solResMap

  -- [Phase] Result
  logPpLnDoc logFilePath $ unlineTimes 
    [ ("Parsing", timeParsing)
    , ("Compiling", timeCompiling)
    , ("Minimize", timeMinimizeCs)
    , ("ConstraintResolution", timeConstraintResolution )
    , ("Extraction", timeExtraction)]

-- [Phase 5] Interpret
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

