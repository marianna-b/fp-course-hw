{-# LANGUAGE Strict #-}
{-# OPTIONS -Wall #-}
module ArrayList
    (
      ArrayList
    , pushBack
    , popBack
    , setElem
    , getElem
    , fromList
    , toList
    , emptyArray
    , getBounds
    ) where

import qualified Control.Monad.ST as ST
import qualified Data.Array.ST    as STA
import           Data.Foldable    (traverse_)
import qualified Data.STRef       as STRef

type ArrayList s a = STRef.STRef s (Int, STA.STArray s Int a)

copy :: STA.STArray s Int a -> STA.STArray s Int a -> ST.ST s ()
copy a1 a2 = do
  l <- STA.getAssocs a1
  traverse_ (uncurry $ STA.writeArray a2) l
  return ()

expand :: (Int, STA.STArray s Int a) -> ST.ST s (Int, STA.STArray s Int a)
expand (s, a) = do
  (_, r) <- STA.getBounds a
  if r == s then do
    arr <- STA.newArray_ (1, 3 * s `div` 2 + 1)
    copy a arr
    return (s, arr)
  else
    return (s, a)

pushBack :: a -> ArrayList s a -> ST.ST s ()
pushBack e r = do
  (s, a) <- STRef.readSTRef r >>= expand
  STA.writeArray a (s + 1) e
  STRef.writeSTRef r (s + 1, a)

popBack :: ArrayList s a -> ST.ST s ()
popBack a = do
  (l, r) <- STRef.readSTRef a
  STRef.writeSTRef a (l - 1, r)

getElem :: ArrayList s a -> Int -> ST.ST s a
getElem a i = do
  (_, l) <- STRef.readSTRef a
  (_, r) <- STA.getBounds l
  if i > r then error "Index out of range" else STA.readArray l i

setElem :: ArrayList s a -> Int -> a -> ST.ST s ()
setElem a i e = do
  l <- snd <$> STRef.readSTRef a
  (_, r) <- STA.getBounds l
  if i > r then error "Index out of range" else STA.writeArray l i e

fromList :: [a] -> ST.ST s (ArrayList s a)
fromList l = do
  x <- STA.newListArray (1, length l) l
  STRef.newSTRef (length l, x)

toList :: ArrayList s a -> ST.ST s [a]
toList a = do
  (s, l) <- STRef.readSTRef a
  take s <$> STA.getElems l

emptyArray :: ST.ST s (ArrayList s a)
emptyArray = (\x -> (0, x)) <$> STA.newArray_ (1, 10) >>= STRef.newSTRef

getBounds :: ArrayList s a -> ST.ST s (Int, Int)
getBounds a = do
  (s, _) <- STRef.readSTRef a
  return (1, s)
