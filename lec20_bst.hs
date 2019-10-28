import Data.List
import Spoooky
{- aCdata BST = Empty | Node Integer BST BST deriving Show
data BSTC = Empty | Node Char BSTC BSTC deriving Show-}
data BST a = Empty | Node a (BST a) (BST a) deriving Show

treeA = Node 7 (Node 3 Empty Empty) (Node 10 Empty Empty)
treeB = Node 3 Empty (Node 7 Empty (Node 10 Empty Empty))

contents :: BST a -> [a]
contents Empty = []
contents (Node x lft rgt) = [x] ++ (contents lft) ++ (contents rgt)

instance Ord a => Eq (BST a) where
   (==) treeOne treeTwo = sort (contents treeOne) == sort (contents treeTwo)
