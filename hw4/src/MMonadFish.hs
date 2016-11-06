{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module MMonadFish where

import qualified Monads

instance Monads.Monad m =>
         Monads.MonadFish m where
    returnFish = Monads.return
    (f >=> g) x = f x Monads.>>= g
