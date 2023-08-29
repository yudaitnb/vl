module Graph (
  makeGraph, distEdges, verticies, edges, from, deg, adjacents, concatGraph,
  deleteNode, deleteEdge,
  Graph(..), tsort, tsortBy
) where

import Data.List
import Control.Monad.State
import Data.Maybe (fromMaybe)
import Prelude hiding (log)

-- import Algebra.Graph

data Graph a =
    EmptyGraph
  | Graph ([Node a], [Edge a])
  deriving (Eq, Ord, Show)
type Node a = a
type Edge a = (a,a)
type Path a = [Node a]

-- Generate graph by using connected edges
makeGraph :: Eq a => [(a, a)] -> Graph a
makeGraph lst =
  let nodes = nub $ map fst lst
      edges = lst
  in Graph (nodes,edges)

distEdges :: (Eq a) => [(a, [a])] -> [Edge a]
distEdges = concatMap (\(x, lst) -> map (x,) lst)

verticies :: Graph a -> [Node a]
verticies EmptyGraph = []
verticies (Graph (vs, _)) = vs

edges :: Graph a -> [Edge a]
edges EmptyGraph = []
edges (Graph (_, edges)) = edges

from :: (Eq a) => Node a -> [Edge a] -> [Edge a]
from n = filter (\(s,_) -> s == n)

deg :: (Eq a) => Node a -> [Edge a] -> Int
deg n es = length $ from n es

isSink :: (Eq a) => [Edge a] -> Node a -> Bool
isSink es n = null (from n es)

adjacents :: (Eq a) => Node a -> [Edge a] -> [Node a]
adjacents n es = map snd $ from n es

concatGraph :: Graph a -> Graph a -> Graph a
concatGraph EmptyGraph g = g
concatGraph g EmptyGraph = g
concatGraph (Graph (n1, e1)) (Graph (n2, e2)) = Graph (n1++n2, e1++e2)

deleteNode :: Eq a => Node a -> Graph a -> Graph a
deleteNode n g =
  let newNodes = filter (/= n) (verticies g)
      newEdges = filter (\e' -> fst e' /= n && snd e' /= n) (edges g)
  in Graph (newNodes, newEdges)

deleteEdge :: Eq a => Edge a -> Graph a -> Graph a
deleteEdge e g =
  let newNodes = verticies g
      newEdges = filter (/= e) (edges g)
  in Graph (newNodes, newEdges)

--------------------

-- 事前条件: グラフgは有向サイクルを含まない (DAG)
-- https://algo-method.com/tasks/963
-- ghci> tsort $ makeGraph [(0,5),(1,3),(1,6),(2,5),(2,7),(3,0),(3,7),(4,1),(4,2),(4,6),(6,7),(7,0)]
-- [4,2,1,6,3,7,0,5]
-- ghci> tsort $ makeGraph [(4,2),(0,5),(1,4),(3,5),(3,1),(1,0),(0,2)]
-- [3,1,0,5,4,2]
-- ghci> tsort $ Graph ([0..3], [(3,2),(1,2)])
-- [3,1,2,0]

data TEortEnv' a = TEortEnv'
  { black :: [Node a]
  , order :: [Node a]
  }
  deriving (Eq, Ord, Show)
type TEortEnv b a = State (TEortEnv' b) a

drawBlack :: (Eq a) => Node a -> TEortEnv a ()
drawBlack n = modify $ \env@(TEortEnv' b o) -> TEortEnv' (n:b) o

isBlack :: (Eq a) => Node a -> TEortEnv a Bool
isBlack n = state $ \env@(TEortEnv' b _) -> (n `elem` b, env)

addOrder :: Node a -> TEortEnv a ()
addOrder n = modify $ \env@(TEortEnv' b o) -> TEortEnv' b (n:o)

getOrder :: TEortEnv a [Node a]
getOrder = state $ \env@(TEortEnv' _ o) -> (o, env)

tsort :: Eq (Node a) => Graph a -> [Node a]
tsort g = order $ execState (tsortWithTEortEnv g) (TEortEnv' [] [])
  where
    tsortWithTEortEnv :: Eq (Node a) => Graph a -> TEortEnv a ()
    tsortWithTEortEnv g = do
      forM_ vs $ \v -> do
        b <- isBlack v
        unless b (rec_ v)
      where
        vs = verticies g
        es = edges g
        rec_ v = do
          drawBlack v
          forM_ (adjacents v es) $ \i -> do
            b <- isBlack i
            unless b (rec_ i)
          addOrder v

-- ^ リストをグラフの順序に従い並び替える
-- ^ 事前条件
-- ^ (1) map getN lst ⊆ verticies graph
-- ^ (2) graphは有向サイクルを含まない(DAG)
tsortBy :: (Eq a) => Graph a -> (b -> a) -> [b] -> [b]
tsortBy graph getN lst =
  let sorted = reverse $ tsort graph in
  sortOn (\x -> elemIndex (getN x) sorted) lst

---------------------

data CycleOfEnv' a = CycleOfEnv'
  { seen :: [Node a]
  , finished :: [Node a]
  , log :: Path a
  , hasCycle :: Bool
  }
  deriving (Eq, Ord, Show)
type CycleOfEnv b a = State (CycleOfEnv' b) a

see :: Node a -> CycleOfEnv a ()
see n = modify $ \env@(CycleOfEnv' s f l h) -> CycleOfEnv' (n:s) f l h

isSeen :: (Eq a) => Node a -> CycleOfEnv a Bool
isSeen n = state $ \env@(CycleOfEnv' s _ _ _) -> (n `elem` s, env)

finish :: Node a -> CycleOfEnv a ()
finish n = modify $ \env@(CycleOfEnv' s f l h) -> CycleOfEnv' s (n:f) l h

isFinished :: (Eq a) => Node a -> CycleOfEnv a Bool
isFinished n = state $ \env@(CycleOfEnv' _ f _ _) -> (n `elem` f, env)

pushLog :: Node a -> CycleOfEnv a ()
pushLog n = modify $ \env@(CycleOfEnv' s f l h) -> CycleOfEnv' s f (n:l) h

popLog :: CycleOfEnv a (Node a)
popLog = state $ \env@(CycleOfEnv' s f (n:l) h) -> (n, CycleOfEnv' s f l h)

getLog :: CycleOfEnv a (Path a)
getLog = state $ \env -> (log env, env)

setLog :: Path a -> CycleOfEnv a ()
setLog p = modify $ \env -> env { log = p }

flagTrue :: CycleOfEnv a ()
flagTrue = modify $ \env@(CycleOfEnv' s f l _) -> CycleOfEnv' s f l True

flag :: CycleOfEnv a Bool
flag = state $ \env -> (hasCycle env, env)

-- ^ graph上を探索し、サイクルが
-- ^ 存在する 　-> Juste 見つかった最初のサイクル
-- ^ 存在しない -> Nothing
-- ^ https://algo-method.com/tasks/970
-- ghci> cycleOf $ Graph ([0..7], [(0,5),(1,3),(1,6),(2,5),(2,7),(3,0),(3,7),(4,1),(4,2),(4,6),(6,7),(7,0)])
-- [] (サイクル無し)
-- ghci> cycleOf $ Graph ([0..4], [(0,2),(4,1),(1,2),(2,4),(4,3)])
-- [1,4,2]
-- ghci> cycleOf $ Graph ([0..6], [(3,2),(4,1),(2,5),(4,0),(3,6),(0,1)])
-- [] (サイクル無し)
-- ghci> cycleOf $ Graph ([0..7], [(0,5),(3,1),(1,6),(2,5),(2,7),(3,0),(7,3),(4,1),(4,2),(4,6),(6,7),(7,0)])
-- [3,7,6,1]

-- cycleOf :: Eq a => Graph a -> Path a
cycleOf :: Eq a => Graph a -> Path a
cycleOf graph = reverseL . log $ execState (cycleOfWithEnv graph) (CycleOfEnv' [] [] [] False)
  where
    cycleOfWithEnv :: (Eq a) => Graph a -> CycleOfEnv a ()
    cycleOfWithEnv g = do
      forM_ vs $ \v -> do
        flag >>= \f -> if f
          then do return ()
          else do isSeen v >>= \b -> unless b (rec_ v)
      where
        vs = verticies g
        es = edges g
        rec_ v = do
          pushLog v
          see v
          forM_ (adjacents v es) $ \i -> do
            (&&) <$> (not <$> isFinished i) <*> isSeen i >>= \b ->
              when b $ do
                pushLog i
                void flagTrue
            flag >>= \f -> unless f $
              isSeen i >>= \b -> unless b (rec_ i)
          flag >>= \f -> unless f $ do
              void popLog
              finish v
    -- サイクル抽出
    reverseL :: Eq a => Path a -> Path a
    reverseL [] = []
    reverseL (h:xs) = h : takeWhile (/= h) xs