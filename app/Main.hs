module Main where

import System.FilePath ( splitFileName )
import System.Environment ( getArgs )
import System.Directory ( createDirectoryIfMissing, removeFile )

import Parser ( parseAST )
import Language.Haskell.Exts.ExactPrint ( exactPrint )
-- import Syntax.LambdaVL
import Syntax.Absyn
import Renamer
import Girard ( girardFwd )
import Util

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
  removeFile logfile

  -- output
  ast <- parseAST path
  log "=== Exactprint ==="
  log $ exactPrint ast []
  log "=== AST (Syntax.Absyn) ==="
  print ast
  log ast
  log "=== Alpha renaming ==="
  let ast_renamed = alphaRename ast
  log ast_renamed
  log "=== AST (Syntax.VL) ==="
  let ast_vl = girardFwd ast_renamed
  log ast_vl

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