module Tree where

data Tree a =
    Leaf |
    Tree (Tree a) a (Tree a)
    deriving (Show)

empty :: Tree a
empty = Leaf

insert :: (Ord a) => a -> Tree a -> Tree a
insert key Leaf = Tree Leaf key Leaf
insert key (Tree l k r) =
    if key < k
        then Tree (insert key l) k r
        else if key > k
                then Tree l k (insert key r)
                else Tree l key r

member :: (Ord a) => a -> Tree a -> Bool
member _ Leaf = False
member key (Tree l k r) =
    if key == k
        then True
        else member key l || member key r
