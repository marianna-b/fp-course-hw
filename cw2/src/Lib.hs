{-# OPTIONS -Wall #-}
module Lib
    ( safeHead
    , Distances
    , get
    , put
    , runFloyd
    , mainFloyd
    ) where

import qualified Data.Array.IO as A
import qualified Control.Monad.Trans.Maybe as M
import qualified Control.Monad.Trans as T
import qualified Data.Foldable as F

safeHead :: Foldable t => t a -> Maybe a
safeHead = foldr (\x _ -> Just x) Nothing

type Distances = A.IOArray (Int, Int) Int

get :: Distances -> Int -> Int -> M.MaybeT IO Int
get dist u v = do
  a <- T.lift $ A.readArray dist (u, v)
  if a == 0 then M.MaybeT (return Nothing) else return a

put :: Int -> Int -> Distances -> M.MaybeT IO ()
put u v dist = T.lift $ A.writeArray dist (u, v) 1

putVal :: Int -> Int -> Distances -> Int -> M.MaybeT IO ()
putVal u v dist val = T.lift $ A.writeArray dist (u, v) val

runFloyd' :: Int -> Int -> Int -> Distances -> M.MaybeT IO ()
runFloyd' i j k dist = do
  ans <- (\x y z -> if x < y + z then Just (y + z) else Nothing) <$> get dist i j <*> get dist i k <*> get dist k j
  F.forM_ ans (putVal i j dist)

runFloyd :: Int -> Int -> Int -> Int -> Distances -> M.MaybeT IO ()
runFloyd i j k n dist
  | i > n || j > n || k > n = return ()
  | otherwise = do
    runFloyd' i j k dist
    runFloyd (i + 1) j k n dist
    runFloyd i (j + 1) k n dist
    runFloyd i j (k + 1) n dist

mainFloyd :: Int -> IO ()
mainFloyd n = do
  arr <- A.newArray ((1, 1), (n, n)) 0
  _ <- M.runMaybeT $ runFloyd 0 0 0 n arr
  return ()
