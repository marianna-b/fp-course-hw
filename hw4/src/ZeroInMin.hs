{-# OPTIONS -Wall #-}
module ZeroInMin
       ( manHeaps
       , zeroInMin
       ) where

manHeaps :: (Int, Int) -> [(Int, Int)]
manHeaps (a, b) = filter isCorrectHeaps
    [ (a - 1, b    ), (a   *   2, b `div` 2)
    , (a    , b - 1), (a `div` 2, b   *   2)
    ]
  where
    isCorrectHeaps (x, y) = x >= 0 && y >= 0

zeroInMin' :: [(Int, Int)] -> Int -> Int
zeroInMin' h x =
    if any check h
        then x + 1
        else zeroInMin' (h >>= manHeaps) $ x + 1
  where
    check (a,b) = a == 0 && b == 0

zeroInMin :: (Int, Int) -> Int
zeroInMin h = zeroInMin' [h] 0
