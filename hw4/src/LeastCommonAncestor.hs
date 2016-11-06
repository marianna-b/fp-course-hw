{-# OPTIONS -Wall #-}
module LeastCommonAncestor
       ( InNode(Node, label, parent)
       , leastCommonAncestor
       ) where

import           Data.List (find)

data InNode a = Node
    { label :: a
    , parent :: Maybe (InNode a)
    } deriving (Eq,Show)

check :: Eq a => Maybe a -> [a] -> Maybe a
check x xs = x >>= (\t -> find (== t) xs)

lca :: Eq a => Maybe (InNode a) -> Maybe (InNode a) -> [InNode a] -> [InNode a] -> Maybe (InNode a)
lca x y xs ys =
    case (check x ys, check y xs) of
        (Just a, _) -> Just a
        (_, Just b) -> Just b
        (Nothing, Nothing) ->
            case (x >>= parent, y >>= parent) of
                (Nothing, Nothing) -> Nothing
                (Just a, Just b) -> lca (Just a) (Just b) (a : xs) (b : ys)
                (_, Just b) -> lca x (Just b) xs (b : ys)
                (Just a, _) -> lca (Just a) y (a : xs) ys

leastCommonAncestor :: Eq a => InNode a -> InNode a -> Maybe (InNode a)
leastCommonAncestor a b = lca (Just a) (Just b) [a] [b]
