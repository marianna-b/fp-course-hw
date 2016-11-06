{-# OPTIONS -Wall #-}
{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module Tuple where

import qualified Data.Monoid as M

class Functor f  where
  fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

class Foldable t where
  foldMap :: M.Monoid m => (a -> m) -> t a -> m

class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

instance Functor ((,) a) where
    fmap f (x, y) = (x, f y)

instance M.Monoid a => Applicative ((,) a) where
  pure x = (M.mempty, x)
  (a, f) <*> (b, x) = (M.mappend a b, f x)

instance Foldable ((,) a) where
  foldMap f (_, y) = f y

instance Traversable ((,) a) where
  traverse f (x, y) = (, ) x `fmap` f y

