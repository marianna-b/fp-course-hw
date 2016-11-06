{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module MonadFishJoin where

import           Data.Function (id, const)

import qualified Monads

instance Monads.MonadFish m =>
         Monads.MonadJoin m where
    returnJoin = Monads.returnFish
    join x = (const x Monads.>=> id) x
