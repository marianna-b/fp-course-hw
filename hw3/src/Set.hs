{-# OPTIONS -Wall #-}
module Set where

import qualified Tree as T

class Set t where
    emptySet :: Ord a => t a
    toList   :: Ord a => t a -> [a]
    find     :: Ord a => t a -> a -> Maybe a
    insert   :: Ord a => t a -> a -> t a
    delete   :: Ord a => t a -> a -> t a
    next     :: Ord a => t a -> a -> Maybe a
    fromList :: Ord a => [a] -> t a

instance Set T.BinarySearchTree where
    emptySet = T.Leaf
    toList = T.toList
    find = T.find
    insert = T.insert
    delete = T.delete
    next T.Leaf _ = Nothing
    next (T.Node x l r) val
      | x > val =
          case next l val of
              Nothing -> Just x
              res -> res
      | otherwise = next r val
    fromList = T.fromList
