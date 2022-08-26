module Main where

import System.FilePath ( splitFileName )
import System.Environment ( getArgs )

import Parser ( parseAST )
import Language.Haskell.Exts.ExactPrint ( exactPrint )
-- import Syntax.LambdaVL
import Syntax.Absyn
import Desugar
import Renamer
import Girard ( girardFwd )
import TypeInference
import Util
import Desugar

-- from hint
-- import qualified Language.Haskell.Interpreter as H
-- import Language.Haskell.Interpreter.Unsafe

main :: IO ()
main = do
  [path, func] <- getArgs
  let (dir, fn) = splitFileName path
      logdir = "./log/"
      logfile = logdir ++ fn ++ ".log"
      log :: Pretty a => a -> IO ()
      log = logPpLn logfile

  -- create log directory 
  createDirectoryIfMissing False logdir
  removeFileIfExists logfile

  -- output
  ast <- parseAST path
  log "\n=== Exactprint ==="
  log $ exactPrint ast []
  log "\n=== AST (Syntax.Absyn) ==="
  print ast
  log ast
  -- log "=== Alpha renaming ==="
  -- let ast_renamed = alphaRename ast
  -- log ast_renamed
  -- log "\n=== AST (Syntax.VL) ==="
  -- let ast_vl = girardFwd ast_renamed
  -- -- let ast_vl = girardFwd ast
  -- log ast_vl
  -- log "\n=== Types (Syntax.VL) ==="
  -- let types = getInterface ast_vl
  -- log types

  log "\n=== Desugared AST (Syntax.STLC) ==="
  let ast_desugared = desugarAST ast
  log ast_desugared
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