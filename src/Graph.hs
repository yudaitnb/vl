module Graph (
  tsort, makeGraph,
  Edge(..), Node(..), Graph(..)
) where

import Data.List
import Control.Monad.State

newtype Graph a = Graph ([Node a], [Edge a]) deriving (Eq, Ord, Show)
type Node a = a
type Edge a = (a,a)

-- Generate graph by using connected edges
makeGraph :: Eq a => [(a, a)] -> Graph a
makeGraph lst =
  let nodes = nub $ map fst lst
      edges = lst
  in Graph (nodes,edges)

verticies :: Graph a -> [Node a]
verticies (Graph (vs, _)) = vs

edges :: Graph a -> [Edge a]
edges (Graph (_, edges)) = edges

from :: (Eq a) => Node a -> [Edge a] -> [Edge a]
from n = filter (\(s,_) -> s == n)

deg :: (Eq a) => Node a -> [Edge a] -> Int
deg n es = length $ from n es

isSink :: (Eq a) => [Edge a] -> Node a -> Bool
isSink es n = null (from n es)

adjacents :: (Eq a) => Node a -> [Edge a] -> [Node a]
adjacents n es = map snd $ from n es

data Env' a = Env'
  { black :: [Node a]
  , order :: [Node a]
  }
  deriving (Eq, Ord, Show)
type Env b a = State (Env' b) a

drawBlack :: (Eq a) => Node a -> Env a ()
drawBlack n = state $ \env@(Env' b o) -> ((), Env' (n:b) o)

isBlack :: (Eq a) => Node a -> Env a Bool
isBlack n = state $ \env@(Env' b _) -> (n `elem` b, env)

addOrder :: Node a -> Env a ()
addOrder n = state $ \env@(Env' b o) -> ((), Env' b (n:o))

getOrder :: Env a [Node a]
getOrder = state $ \env@(Env' _ o) -> (o, env)

--------------------

-- 事前条件: グラフgは有効サイクルを含まない
-- https://algo-method.com/tasks/963
-- ghci> tsort $ makeGraph [(0,5),(1,3),(1,6),(2,5),(2,7),(3,0),(3,7),(4,1),(4,2),(4,6),(6,7),(7,0)]
-- [4,2,1,6,3,7,0,5]
-- ghci> tsort $ makeGraph [(4,2),(0,5),(1,4),(3,5),(3,1),(1,0),(0,2)]
-- [3,1,0,5,4,2]
-- ghci> tsort $ Graph ([0..3], [(3,2),(1,2)])
-- [3,1,2,0]

tsort :: Eq (Node a) => Graph a -> [Node a]
tsort g = order $ execState (tsortWithEnv g) (Env' [] [])
  where
    tsortWithEnv :: Eq (Node a) => Graph a -> Env a ()
    tsortWithEnv g = do
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
