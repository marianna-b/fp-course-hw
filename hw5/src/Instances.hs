{-# OPTIONS -Wall #-}
{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module Instances where

import           Prelude(($), map, (++), (<$>))
import qualified Data.Monoid as M
import           Data.Foldable(Foldable(foldMap, foldr))
import           Data.Traversable(Traversable(traverse))
import           Data.Functor(Functor(fmap))
import           Control.Applicative(Applicative(pure, (<*>)))

newtype Const a b = Const
    { getConst :: a
    }

newtype Identity a = Identity
    { runIdentity :: a
    }

data Either a b
    = Left a
    | Right b

data Tree a = Node
    { rootLabel :: a
    , subForest :: Forest a
    }

type Forest a = [Tree a]

instance Functor (Const a) where
  fmap _ (Const v) = Const v

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Functor Tree where
  fmap f (Node v l) = Node (f v) $ map (fmap f) l

instance Functor (Either a) where
  fmap _ (Left x) = Left x
  fmap f (Right x) = Right $ f x


instance M.Monoid a => Applicative (Const a) where
  pure _ = Const M.mempty
  Const f <*> Const x = Const $ M.mappend f x

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity $ f x

instance Applicative Tree where
  pure x = Node x []
  (Node f fs) <*> t@(Node x xs) = Node (f x) $ map (fmap f) xs ++ map (<*> t) fs

instance Applicative (Either a) where
  pure = Right
  (Right f) <*> (Right x) = Right $ f x
  (Left f) <*> _ = Left f
  _ <*> (Left x) = Left x


instance Foldable (Const a) where
  foldMap _ _ = M.mempty

instance Foldable Identity where
  foldr f b (Identity x) = f x b

instance Foldable Tree where
  foldMap f (Node x xs) = M.mappend (f x) $ foldMap (foldMap f) xs

instance Foldable (Either a) where
  foldMap _ (Left _) = M.mempty
  foldMap f (Right x) = f x


instance Traversable (Const a) where
  traverse _ (Const a) = pure $ Const a

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

instance Traversable Tree where
  traverse f (Node x xs) = Node <$> f x <*> traverse (traverse f) xs

instance Traversable (Either a) where
  traverse _ (Left x) = pure (Left x)
  traverse f (Right x) = Right <$> f x

