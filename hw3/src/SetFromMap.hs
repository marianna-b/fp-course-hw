{-# OPTIONS -Wall #-}
module SetFromMap where

import qualified Map  as M
import qualified Tree as T

class SetFromMap t where
    emptySet :: Ord a => t (M.Node a a)
    toList   :: Ord a => t (M.Node a a) -> [a]
    find     :: Ord a => t (M.Node a a) -> a -> Maybe a
    insert   :: Ord a => t (M.Node a a) -> a -> t (M.Node a a)
    delete   :: Ord a => t (M.Node a a) -> a -> t (M.Node a a)
    next     :: Ord a => t (M.Node a a) -> a -> Maybe a
    fromList :: Ord a => [a] -> t (M.Node a a)

instance SetFromMap T.BinarySearchTree where
  emptySet = M.emptyMap
  toList t = map fst $ M.toList t
  find t x = fst `fmap` M.find t x
  insert t x = M.insert t x x
  delete = M.delete
  next t x = fst `fmap` M.next t x
  fromList xs = M.fromList $ map (\a -> (a, a)) xs
