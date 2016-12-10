{-# OPTIONS -Wall #-}
{-# LANGUAGE Strict #-}
module Main where

import qualified ArrayList
import           Control.Monad    (forM_)
import qualified Control.Monad.ST as ST
import           Criterion.Main
import qualified Data.Sequence    as S


testArrayList :: ([Int], Int) -> Int
testArrayList (t, i) = ST.runST $ do
  arr <- ArrayList.fromList t
  ArrayList.pushBack i arr
  ArrayList.popBack arr
  ArrayList.popBack a
  ArrayList.setElem arr i 0
  ArrayList.getElem arr $ i + 1

testSequence :: ([Int], Int) -> Int
testSequence (t, i) = let a = S.fromList t S.|> i in
  S.index (S.update i 0 (S.take (S.length a - 2) a)) (i + 1)

testList :: ([Int], Int) -> Int
testList (t, i) = let a = reverse $ drop 2 (1:reverse t) in
  let (l, r) = splitAt i a in
    (init l ++ (0:r)) !! i + 1

initTestArray :: Int -> ()
initTestArray n = ST.runST $ do
  arr <- ArrayList.emptyArray
  forM_ [1..n] $ \i -> ArrayList.pushBack i arr

initTestSequence :: Int -> S.Seq Int
initTestSequence n = S.iterateN n (+ 1) 1

reverseArray :: [Int] -> ()
reverseArray x = ST.runST $ do
  a <- ArrayList.fromList x
  (l, r) <- ArrayList.getBounds a
  forM_ [l..(r `div` 2)] $ \i -> do
    w1 <- ArrayList.getElem a i
    w2 <- ArrayList.getElem a (r + 1 - i)
    ArrayList.setElem a i w2
    ArrayList.setElem a (r + 1 - i) w1

mapArray :: (Int -> Int) -> [Int] -> ()
mapArray f x = ST.runST $ do
  a <- ArrayList.fromList x
  (l, r) <- ArrayList.getBounds a
  forM_ [l..r] $ \i -> do
    w1 <- ArrayList.getElem a i
    ArrayList.setElem a i $ f w1

main :: IO ()
main = defaultMain
  [ bgroup "init5"
    [ bench "seq"  $ nf initTestSequence 5
    , bench "arr"  $ nf initTestArray 5
    ],
    bgroup "init1999"
    [ bench "seq" $ nf testSequence ([1..1000], 500)
    , bench "arr" $ nf testArrayList ([1..1000], 500)
    , bench "lst" $ nf testList ([1..1000], 500)
    ],
    bgroup "reverse"
    [ bench "seq" $ nf (S.reverse . S.fromList) ([1..1000]::[Int])
    , bench "arr" $ nf reverseArray [1..1000]
    ],
    bgroup "reverse"
    [ bench "arr" $ nf (mapArray (*2)) [1..1000]
    , bench "seq" $ nf (S.mapWithIndex (\_ x -> 2 * x) . S.fromList) ([1..1000]::[Int])
    ]
  ]
