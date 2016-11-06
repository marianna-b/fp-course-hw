{-# OPTIONS -Wall #-}
module BinomialHeap where

data BinomialTree a = BTree a Int [BinomialTree a]
  deriving Show
type BinomialHeap a = [BinomialTree a]

findMin :: Ord a => BinomialHeap a -> Maybe a
findMin [] = Nothing
findMin [BTree k _ _] = Just k
findMin (BTree k _ _:xs) =  min k `fmap` findMin xs

combine :: Ord a => BinomialTree a -> BinomialTree a -> BinomialTree a
combine x@(BTree k1 s1 l1) y@(BTree k2 s2 l2)
  | s1 /= s2 = error "Combining heaps of different degree"
  | k1 < k2 = BTree k1 (s1 + 1) $ y:l1
  | otherwise = BTree k2 (s2 + 1) $ x:l2

processCarry :: Ord a => BinomialTree a -> (Maybe (BinomialTree a), BinomialHeap a) -> (Maybe (BinomialTree a), BinomialHeap a)
processCarry t (Nothing, l) = (Nothing, t:l)
processCarry x@(BTree _ s1 _) (Just y@(BTree _ s2 _), l)
  | s1 == s2 = (Just (combine x y), l)
  | otherwise = (Nothing, x:y:l)

merge' :: Ord a => BinomialHeap a -> BinomialHeap a -> (Maybe (BinomialTree a), BinomialHeap a)
merge' x [] = (Nothing, x)
merge' [] y = (Nothing, y)
merge' (x@(BTree _ s1 _):xs) (y@(BTree _ s2 _):ys)
  | s1 > s2 = processCarry x $ merge' xs $ y:ys
  | s1 < s2 = processCarry y $ merge' (x:xs) ys
  | otherwise = case merge' xs ys of
      (Nothing, l) -> (Just (combine x y), l)
      (Just v, l) -> (Just (combine x y), v:l)

merge :: Ord a => BinomialHeap a -> BinomialHeap a -> BinomialHeap a
merge x y = case merge' x y of
  (Nothing, l) -> l
  (Just v, l) -> v:l

insert :: Ord a => BinomialHeap a -> a -> BinomialHeap a
insert h a = merge h [BTree a 0 []]

extractFromHeap :: Ord a => BinomialHeap a -> a -> (BinomialHeap a, BinomialTree a)
extractFromHeap [] _ = error "Can't extract from empty heap"
extractFromHeap (x@(BTree k _ _):xs) m
  | k == m = (xs, x)
  | otherwise = (x:a, b)
    where (a, b) = extractFromHeap xs m

deleteMin :: Ord a => BinomialHeap a -> BinomialHeap a
deleteMin h = case findMin h of
  Nothing -> h
  Just m -> let (h1, BTree _ _ l) = extractFromHeap h m in
   merge h1 l
