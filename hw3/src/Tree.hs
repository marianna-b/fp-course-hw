{-# OPTIONS -Wall #-}
module Tree where

data BinarySearchTree a = Leaf | Node a (BinarySearchTree a) (BinarySearchTree a)
  deriving Show

find :: Ord a => BinarySearchTree a -> a -> Maybe a
find Leaf _ = Nothing
find (Node val left right) key
  | val == key = Just val
  | val < key = find right key
  | otherwise = find left key

insert :: Ord a => BinarySearchTree a -> a -> BinarySearchTree a
insert Leaf key = Node key Leaf Leaf
insert x@(Node val left right) key
  | key < val = Node val (insert left key) right
  | key > val = Node val left (insert right key)
  | otherwise = x

getMin :: Ord a => BinarySearchTree a -> a
getMin Leaf = error "No minimum in empty tree"
getMin (Node val Leaf _) = val
getMin (Node _ left _) = getMin left

delete :: Ord a => BinarySearchTree a -> a -> BinarySearchTree a
delete Leaf _ = Leaf
delete (Node val left right) key
  | key < val = Node val (delete left key) right
  | key > val = Node val left (delete right key)
  | otherwise = case (left, right) of
                  (Leaf, Leaf) -> Leaf
                  (_, Leaf) -> left
                  (Leaf, _) -> right
                  _ -> Node newVal left $ delete right newVal
                    where newVal = getMin right

toList :: Ord a => BinarySearchTree a -> [a]
toList Leaf = []
toList (Node val left right) = val : toList left ++ toList right

fromList :: Ord a => [a] -> BinarySearchTree a
fromList [] = Leaf
fromList (x:xs) = Node x (fromList l) $ fromList r
  where l = filter (x >) xs
        r = filter (> x) xs

split :: Ord a => BinarySearchTree a -> a -> (BinarySearchTree a, BinarySearchTree a)
split Leaf _ = (Leaf, Leaf)
split (Node x l r) v
  | x == v = (l, r)
  | x < v = let (a, b) = split r v in
              (Node x l a, b)
  | otherwise = let (a, b) = split l v in
              (a, Node x b r)

instance Ord a => Monoid (BinarySearchTree a) where
  mempty = Leaf
  mappend Leaf x = x
  mappend x Leaf = x
  mappend n1@(Node x l1 r1) n2@(Node y l2 r2)
    | x < y     = let (a, b) = split n2 x in
                    Node x (mappend l1 a) (mappend r1 b)
    | otherwise = let (a, b) = split n1 y in
                    Node y (mappend l2 a) (mappend r2 b)

instance Foldable BinarySearchTree where
  foldr _ b Leaf = b
  foldr f b (Node x l r) = foldr f (foldr f (f x b) r) l


