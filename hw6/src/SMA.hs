{-# OPTIONS -Wall #-}
module SMA
    ( moving
    ) where

import           Control.Monad.State(State, evalState, state)

type SMAState a = ([a], Int)

recalc :: (Fractional a, Show a) => [a] -> a -> Int -> ([a], Int)
recalc l new n
  | len < n = (new:l, len + 1)
  | len == n = (new:init l, n)
  | otherwise = error "List can't be longer than n"
    where len = length l

smaState :: (Show a, Fractional a) => a -> [a] -> State (SMAState a) [a]
smaState toAdd oldAns = state $ \(curr, n) ->
                     let (newCurr, amount) = recalc curr toAdd n
                         newAns = sum newCurr / fromIntegral amount in
                       (newAns:oldAns, (newCurr, n))

moving :: (Show a, Fractional a) => Int -> [a] -> [a]
moving _ [] = []
moving n (x:xs) = reverse $
    evalState
        (foldl (\s v -> s >>= smaState v) (smaState x []) xs)
        ([], n)
