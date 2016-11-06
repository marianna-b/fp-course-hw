{-# OPTIONS -Wall #-}
module HW1 where

import Data.List(foldl')
import System.Random (newStdGen, randomRs)

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

{-- 1 --}

readInt :: String -> Int
readInt ('+':'-':_) = error "Incorrect expression"
readInt ('+':xs) = read xs
readInt xs = read xs

stringSum :: String -> Int
stringSum = foldl' (+) 0 . map readInt . words

{-- 2 --}

takeRow :: [[a]] -> ([a], [[a]])
takeRow [] = ([], [])
takeRow ([]:xs) = ([], []:xs)
takeRow (x:xs) = (head x:a, tail x:b)
  where (a, b) = takeRow xs

zipN :: ([a] -> b) ->[[a]] -> [b]
zipN _ [] = []
zipN f xs = if null a then [] else f a : zipN f b
  where (a, b) = takeRow xs

{-- 3 --}

merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort l = mergeSort left `merge` mergeSort right
  where (left, right) = splitAt (length l `div` 2) l
