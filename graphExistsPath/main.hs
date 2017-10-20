module Graph where

import Control.Monad
import Control.Monad.State
import qualified Data.Set as S

type Node = Char
type Arc  = (Node, Node)


-- 1. Simple Solution
solveGraph :: Node -> Node -> [Arc] -> Bool
solveGraph s e arcs = S.member e visited
  where
    visited = visit arcs [s] $ S.singleton s

visit :: [Arc] -> [Node] -> S.Set Node ->  S.Set Node
visit _ [] visited = visited
visit arcs (n:ns) visited = visit arcs (ns ++ newOpen) newVisited 
  where
    es = S.fromList $ expandOnce arcs n
    newVisited = S.union visited es
    newOpen = S.toList $ S.difference es visited

expandOnce :: [Arc] -> Node -> [Node]
expandOnce arcs s = [b | (a,b) <- arcs, a == s]

-- 2. The same solution using the State Monad
-- solveGraph :: Node -> Node -> [Arc] -> Bool
-- solveGraph s e arcs = S.member e visited
--   where
--     visited = snd $ runState (visit arcs [s]) $ S.singleton s

-- visit :: [Arc] -> [Node] -> State (S.Set Node) [Node]
-- visit _ [] = return []
-- visit arcs (n:ns) = do
--   visited <- get
--   let es = S.fromList $ expandOnce arcs n
--   let newVisited = S.union visited es
--   let newOpen = S.toList $ S.difference es visited
--   put newVisited
--   visit arcs $ ns ++ newOpen

-- expandOnce :: [Arc] -> Node -> [Node]
-- expandOnce arcs s = [b | (a,b) <- arcs, a == s]
