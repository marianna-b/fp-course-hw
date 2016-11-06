{-# OPTIONS -Wall #-}
module Tree where

import           Control.Monad.Writer (Writer, runWriter, execWriter, writer, tell)

data BST a = Leaf | Node a (BST a) (BST a)
  deriving Show

type BSTWriter a = Writer String (BST a)

find :: (Ord a, Show a) => BST a -> a -> Writer String (Maybe a)
find Leaf _ = writer (Nothing, "Reached leaf, value not found\n")
find (Node val left right) key
  | val == key = writer (Just val, "Value found " ++ show val)
  | val < key = tell ("Searching in right child of " ++ show val ++ "\n") >> find right key
  | otherwise = tell ("Searching in left child of " ++ show val ++ "\n") >> find left key

insert :: (Ord a, Show a) => BST a -> a -> BSTWriter a
insert Leaf key = writer (Node key Leaf Leaf, "Create new node " ++ show key ++ "\n")
insert x@(Node val left right) key
  | key < val = tell ("Insert into left child of " ++ show val ++ "\n") >> insert left key >>= \l -> return $ Node val l right
  | key > val = tell ("Insert into right child of " ++ show val ++ "\n") >> insert right key >>= \r -> return $ Node val left r
  | otherwise = writer (x, "Value found, no insertion " ++ show val ++ "\n")

getMin :: (Ord a, Show a) => BST a -> Writer String a
getMin Leaf = error "No minimum in empty tree"
getMin (Node val Leaf _) = writer (val, "Minimum found " ++ show val ++ "\n")
getMin (Node val left _) = tell ("Left child exists, search left of " ++ show val ++ "\n") >> getMin left

delete :: (Ord a, Show a) => BST a -> a -> BSTWriter a
delete Leaf _ = writer (Leaf, "Reached leaf, value not found\n")
delete (Node val left right) key
  | key < val = tell ("Delete in left child " ++ show val ++ "\n") >> delete left key >>= \x -> return $ Node val x right
  | key > val = tell ("Delete in right child " ++ show val ++ "\n") >> delete right key >>= \x -> return $ Node val left x
  | otherwise = case (left, right) of
                  (Leaf, Leaf) -> writer (Leaf, "Deleted node " ++ show val ++ "\n")
                  (_, Leaf) -> writer (left, "Deleted " ++ show key ++ " replaced node with left child\n")
                  (Leaf, _) -> writer (right, "Deleted " ++ show key ++ " replaced node with right child\n")
                  _ -> tell ("Replace node with minimum from right child " ++ show key ++ "\n") >>
                    getMin right >>= \x ->
                      (tell ("Delete minimum " ++ show x ++ " from right child " ++ show right ++ "\n") >>
                        delete right x >>= \y -> return $ Node x left y)

toList :: (Show a, Ord a) => BST a -> Writer String [a]
toList Leaf = writer ([], "Leaf found\n")
toList (Node val left right) = tell ("Add value " ++ show val ++ "\n") >> toList left >>= \x -> toList right >>= \y -> return $ val : x ++ y

fromList :: (Show a, Ord a) => [a] -> BSTWriter a
fromList [] = writer (Leaf, "Empty list\n")
fromList (x:xs) = tell ("Create node " ++ show x ++ "\n") >> fromList l >>= \left -> fromList r >>= \right -> return $ Node x left right
  where l = filter (x >) xs
        r = filter (> x) xs
