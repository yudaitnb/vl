module Parser (
  parseAST,
  parseString,
  parseDependencyGraph,
  cvtExtMods,
  VLMod(..),
  ParsedAST(..),
  isValidVersion,
) where

import System.Directory
import Text.Regex.Posix ((=~))

import Control.Monad.Trans (MonadIO(..))
import Control.Monad.Trans.State (modify)
import Control.Monad.State
    ( forM_, execStateT, MonadState(..), MonadTrans(..), StateT(..) )

import Data.List ( filter, map, (\\), nub )
import qualified Data.Map as M
import Data.Map ( empty, insert, insertWith, keys, Map )
import qualified Data.List as L
import Data.List.Split ( splitOn )

import Language.Haskell.Exts hiding (parse)
import Language.Absyn

import Graph
import Util

------------

parseAST :: FilePath -> IO (Module SrcSpanInfo)
parseAST path = fromParseResult <$> parseFile path

parseString :: String -> IO (Module SrcSpanInfo)
parseString ss = return $ fromParseResult $ parseFileContents ss

-------------

cvtMods :: [VLMod] -> Map String [Version]
cvtMods []                = empty
cvtMods ((VLMod mn v):ms) = insertWith (++) mn [v] $ cvtMods ms

cvtExtMods :: [VLMod] -> Map String [Version]
cvtExtMods ms = M.delete "Main" $ cvtMods ms

-- Vis http://www.michaelburge.us/2017/09/01/how-to-use-graphviz-in-haskell.html

data Env' = Env' {
    dependency :: Graph VLMod
  , rest :: [VLMod]
  , finished :: Map VLMod (Module SrcSpanInfo)
  }
type Env a = StateT Env' IO a

addDependency :: Graph VLMod -> Env ()
addDependency g = modify $ \env -> 
  Env'
    (concatGraph g (dependency env))
    (rest env)
    (finished env)

pop :: Env VLMod
pop = state $ \env -> (head $ rest env, env)

push :: VLMod -> Env ()
push str = modify $ \env -> env { rest = str : rest env }

getRest :: Env [VLMod]
getRest = state $ \env -> (rest env, env)

-- nub is necessary
addRest :: [VLMod] -> Env ()
addRest lst = modify $ \env -> env { rest = nub $ rest env ++ (lst \\ keys (finished env)) }

finish :: VLMod -> Env ()
finish m = modify $ \env -> env { rest = L.delete m $ rest env }

insertFinished :: VLMod -> Module SrcSpanInfo -> Env ()
insertFinished str mod = modify $ \env ->
  env { finished = insert str mod (finished env) }

deleteTBDNode :: VLMod -> Env ()
deleteTBDNode mod = modify $ \env -> env { dependency = deleteNode mod $ dependency env }

parseDependencyGraph :: String -> FilePath -> String -> FilePath -> IO (VLMod, [VLMod], ParsedAST)
parseDependencyGraph entry dirpath extension logFilePath = do
  let vlModEntry = VLMod entry Root
  state <- execStateT makeDepGraph (Env' EmptyGraph [vlModEntry] empty)
  return (vlModEntry, reverse $ tsort $ dependency state, finished state)
  where
    logP :: PrettyAST a => a -> IO ()
    logP = logPpLn ppP logFilePath
    logPD :: Doc ann -> IO ()
    logPD = logPpLnDoc logFilePath
    makeDepGraph :: Env ()
    makeDepGraph = do
      rest <- getRest
      case rest of
        [] -> liftIO $ logP "[DEBUG] There are no rest modules to parse."
        target@(VLMod mn ver):rst -> do
          liftIO $ logPD $ ppP "[DEBUG] Start parsing" <+> ppP target
          case ver of
            -- parse root module
            Root -> do
              let pathTarget = dirpath ++ mn ++ extension
              parsed <- liftIO $ parseAST pathTarget
              let newEdges = map
                    (\(x, y) -> (target, VLMod y TBD))
                    (getDepEdge parsed)
                  newNodes = [target]
                  imports = map (`VLMod` TBD) $ getImports parsed
                  newGraph = Graph (newNodes, newEdges)
              addRest imports
              addDependency newGraph
              finish target
              insertFinished target parsed
              makeDepGraph
            -- parse external modules
            TBD -> do
              -- ./examples/A/
              let modPath = dirpath ++ mn ++ "/"
              list <- liftIO $ filter isValidVersion <$> listDirectory modPath
              liftIO $ logPD $ ppP "[DEBUG] Versions found :" <+>
                brackets (concatWith (surround $ comma <> space) (map ppP list))
              forM_ list $ \ver -> do
                -- ./examples/A/ ++ verX/ ++ A.hs
                let version = readVersionFromString ver
                    specificVersionPath = modPath ++ ver ++ "/" ++ mn ++ extension
                parsed <- liftIO $ parseAST specificVersionPath
                let newEdges = map
                      (\(x, y) -> (VLMod x version, VLMod y TBD)) $
                      getDepEdge parsed
                    modTarget = VLMod (getName parsed) version
                    newNodes = [modTarget]
                    imports = map (`VLMod` TBD) $ getImports parsed
                    newGraph = Graph (newNodes, newEdges)
                addRest imports
                finish modTarget
                addDependency newGraph
                insertFinished modTarget parsed
              deleteTBDNode target
              finish target
              makeDepGraph
            _ -> error "The version of the parsing target should be undetermined (Root or TBD)."

readVersionFromString :: String -> Version
readVersionFromString str =
  let vers = splitOn "." str in
  if length vers /= 3
    then error "Version number must be in the format `major.minor.patch`."
    else
      let [major, minor, patch] = map (\x -> read x :: Int) vers in
      Version major minor patch

isValidVersion :: String -> Bool
isValidVersion str = (str =~ "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)") :: Bool

-- isLastVersion :: VLMod -> [VLMod] -> Bool
-- isLastVersion mod lst =
--   let VLMod mn _ = mod in
--   Just mod == find (\(VLMod mn' _) -> mn' == mn) lst

---------------------

instance PrettyAST [VLMod] where
  ppE [] = ppE "None"
  ppE mods = concatWith (surround line) $ Prelude.map ppE mods
  ppP [] = ppP "None"
  ppP mods = concatWith (surround line) $ Prelude.map ppP mods