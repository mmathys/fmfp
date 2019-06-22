-- Trees
import Prelude hiding (traverse)

data Tree t = Leaf | Node t (Tree t) (Tree t) deriving Show

bfs :: Tree t -> [t]
bfs t = traverse [t]

traverse :: [Tree t] -> [t]
traverse [] = []
traverse ts = [t | Node t _ _ <- ts] ++ traverse (childrenAll ts)

childrenAll :: [Tree t] -> [Tree t]
childrenAll ts = concatMap (\t -> children t) ts

children :: Tree t -> [Tree t]
children Leaf = []
children (Node _ l r) = [l, r]

sortedTree :: Ord t => Tree t -> Bool
sortedTree Leaf = True
sortedTree (Node e Leaf Leaf) = True
sortedTree (Node e l Leaf) = (nodeValue l) < e && (sortedTree l)
sortedTree (Node e Leaf r) = (nodeValue r) > e && (sortedTree r)
sortedTree (Node e l r) = (nodeValue l) < e && (sortedTree l) && (nodeValue r) > e && (sortedTree r)

nodeValue :: Ord t => Tree t -> t
nodeValue (Node t _ _) = t