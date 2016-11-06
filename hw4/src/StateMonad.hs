{-# OPTIONS -Wall #-}
{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module StateMonad where

import qualified Control.Monad as M
import qualified Control.Applicative as A
import           Prelude (($))


data State s a = State
    { runState :: s -> (a, s)
    }

instance A.Applicative (State s) => M.Monad (State s) where
  return a = State $ \s -> (a, s)
  st >>= f = State $ \s ->
                       let (a, b) = runState st s in
                       let State r = f a in
                       r b
