{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module MonadFishM where

import           Prelude (const)

import qualified Monads

instance Monads.MonadFish m =>
         Monads.Monad m where
    return = Monads.returnFish
    (>>=) x f = (const x Monads.>=> f) x
