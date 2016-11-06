{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module MMonadJoin where

import           Data.Function (id)

import qualified Monads

instance Monads.Monad m => Monads.MonadJoin m where
  returnJoin = Monads.return
  join v = v Monads.>>= id
