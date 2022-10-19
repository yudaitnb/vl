module DependencyGraph where

import System.Directory

import Control.Monad.State
    ( forM_, execStateT, MonadState(state), MonadTrans(lift), StateT )
import Data.List
import Data.Map
import Data.Tuple
import Data.List.Split

import Syntax.Absyn
import Syntax.SrcLoc
import Syntax.Version

import Graph
import Parser
import Util

data VLMod = VLMod
  { modName :: String
  , version :: Version
  }
  deriving (Eq, Ord, Show)

cvtMods :: [VLMod] -> Map String [Version]
cvtMods []                = Data.Map.empty
cvtMods ((VLMod mn v):ms) = Data.Map.insertWith (++) mn [v] $ cvtMods ms

cvtExtMods :: [VLMod] -> Map String [Version]
cvtExtMods ms = Data.Map.delete "Main" $ cvtMods ms

-- Vis http://www.michaelburge.us/2017/09/01/how-to-use-graphviz-in-haskell.html

data Env' = Env'
  { dependency :: Graph VLMod,
    rest :: [VLMod],
    finished :: Map VLMod (Module SrcSpanInfo)
  }
type Env a = StateT Env' IO a

addDependency :: Graph VLMod -> Env ()
addDependency g = state $ \env -> ((),
  Env'
    (concatGraph g (dependency env))
    (rest env)
    (finished env))

pop :: Env VLMod
pop = state $ \env -> (head $ rest env, env)

push :: VLMod -> Env ()
push str = state $ \env@(Env' d r f) -> ((), Env' d (str:r) f)

getRest :: Env [VLMod]
getRest = state $ \env -> (rest env, env)

addRest :: [VLMod] -> Env ()
addRest lst = state $ \env@(Env' d r f) ->
  ((), Env' d (nub $ r ++ (lst Data.List.\\ keys f)) f)

finish :: VLMod -> Env ()
finish m = state $ \env@(Env' d r f) ->
  ((), Env' d (Data.List.delete m r) f)

insertFinished :: VLMod -> Module SrcSpanInfo -> Env ()
insertFinished str mod = state $ \env ->
  let env' = env { finished = Data.Map.insert str mod (finished env) }
  in ((), env')

deleteTBDNode :: VLMod -> Env ()
deleteTBDNode mod = state $ \env -> ((), env { dependency = deleteNode mod $ dependency env })

getDependencyGraph :: String -> String -> String -> IO (VLMod, [VLMod], Map VLMod (Module SrcSpanInfo))
getDependencyGraph entry dirpath extension = do
  let vlModEntry = VLMod entry Root
  state <- execStateT makeDepGraph (Env' EmptyGraph [vlModEntry] empty)
  return (vlModEntry, reverse $ tsort $ dependency state, finished state)
  where
    makeDepGraph :: Env ()
    makeDepGraph = do
      rest <- getRest
      case rest of
        [] -> return ()
        target@(VLMod mn ver):rst -> do
          case ver of
            Root -> do
              let pathTarget = dirpath ++ mn ++ extension
              parsed <- lift $ parseAST pathTarget
              let newEdges = Data.List.map
                    (\(x, y) -> (target, VLMod y TBD))
                    (getDepEdge parsed)
                  newNodes = [target]
                  imports = Data.List.map (`VLMod` TBD) $ getImports parsed
                  newGraph = Graph (newNodes, newEdges)
              addRest imports
              addDependency newGraph
              finish target
              insertFinished target parsed
              makeDepGraph
            TBD -> do
              -- ./examples/A/
              let modPath = dirpath ++ mn ++ "/"
              list <- lift $ listDirectory modPath
              forM_ list $ \ver -> do
                -- ./examples/A/ ++ verX/ ++ A.hs
                let version = readVersionFromString ver
                    specificVersionPath = modPath ++ ver ++ "/" ++ mn ++ extension
                parsed <- lift $ parseAST specificVersionPath
                let newEdges = Data.List.map (\(x, y) -> (VLMod x version, VLMod y TBD)) $ getDepEdge parsed
                    modTarget = VLMod (getName parsed) version
                    newNodes = [modTarget]
                    imports = Data.List.map (`VLMod` TBD) $ getImports parsed
                    newGraph = Graph (newNodes, newEdges)
                addRest imports
                finish modTarget
                addDependency newGraph
                insertFinished modTarget parsed
              deleteTBDNode target
              finish target
              makeDepGraph
            _ -> error "aaa"

readVersionFromString :: String -> Version
readVersionFromString str =
  let vers = Data.List.Split.splitOn "." str in
  if length vers /= 3
    then error "Version number must be in the format `major.minor.patch`."
    else
      let [major, minor, patch] = Data.List.map (\x -> read x :: Int) vers in
      Version major minor patch

-- isLastVersion :: VLMod -> [VLMod] -> Bool
-- isLastVersion mod lst =
--   let VLMod mn _ = mod in
--   Just mod == find (\(VLMod mn' _) -> mn' == mn) lst

---------------------

instance PrettyAST VLMod where
  ppE (VLMod mn v) =
        nest 2 $ parens $ ppE "VLMod" <> line
    <+> ppE mn <> line
    <+> ppE v
  ppP (VLMod mn v) = ppP mn <> ppP "-" <> ppP v

instance PrettyAST [VLMod] where
  ppE [] = ppE "None"
  ppE mods = concatWith (surround line) $ Prelude.map ppE mods
  ppP [] = ppP "None"
  ppP mods = concatWith (surround line) $ Prelude.map ppP mods