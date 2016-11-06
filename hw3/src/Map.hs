{-# OPTIONS -Wall #-}
module Map where

import qualified Tree as T
import qualified Set  as S

data Node a b = Node a b
  deriving Show

instance Eq a => Eq (Node a b) where
  (==) (Node a1 _) (Node a2 _) = a1 == a2

instance Ord a => Ord (Node a b) where
  compare (Node a1 _) (Node a2 _) = compare a1 a2

class Map t where
    emptyMap :: Ord a => t (Node a b)
    toList   :: Ord a => t (Node a b) -> [(a, b)]
    find     :: Ord a => t (Node a b) -> a -> Maybe (a, b)
    insert   :: Ord a => t (Node a b) -> a -> b -> t (Node a b)
    delete   :: Ord a => t (Node a b) -> a -> t (Node a b)
    next     :: Ord a => t (Node a b) -> a -> Maybe (a, b)
    fromList :: Ord a => [(a, b)] -> t (Node a b)

unwrap :: Node a b -> (a, b)
unwrap (Node a b) = (a, b)

instance Map T.BinarySearchTree where
  emptyMap = S.emptySet
  toList t = map unwrap $ S.toList t
  find t x =  unwrap `fmap` S.find t (Node x undefined)
  insert t x y = S.insert t $ Node x y
  delete t x = S.delete t $ Node x undefined
  next t x = unwrap `fmap` S.next t (Node x undefined)
  fromList xs = S.fromList $ map (uncurry Node) xs
