{-# OPTIONS -Wall #-}
module CW where

import Data.List(nub)
import Data.Monoid(Sum
                  , Product(Product))

{-- 1 remove all strings except unique beginning with given char --}

uniqueStr :: Char -> [String] -> [String]
uniqueStr a = nub . filter ((== a) . head) . filter (not . null)

uniqueStrTest1 = uniqueStr 'a' ["", "ababcad", "empty", "blabal", "a", "a"] == ["ababcad", "a"]

uniqueStrTest2 = uniqueStr 'a' [] == []

uniqueStrTest3 = uniqueStr 'y' ["", "ababcad", "empty", "blabal", "a", "a"] == []

{-- 2 convert to String list as show --}

toString :: Show a => [a] -> String
toString [] = "[]"
toString xs = ("[" ++) $ tail $ foldr (\a b -> "," ++ show a ++ b) "]" xs

toStringTest1 = toString ([]::[Int]) == "[]"
toStringTest2 = toString ([1, 2, 4] :: [Int]) == "[1,2,4]"
toStringTest3 = toString ([Just 1, Nothing, Just 4] :: [Maybe Integer]) == "[Just 1,Nothing,Just 4]"
toStringTest4 = toString ["a", "afjfj", "t"] == "[\"a\",\"afjfj\",\"t\"]"

{-- 3 stack and queue using two stacks with pop and push--}


data Stack a = Stack a (Stack a) | Empty
  deriving Show

pop :: Stack a -> Maybe (Stack a, a)
pop Empty = Nothing
pop (Stack x xs) = Just (xs, x)

push :: Stack a -> a -> Stack a
push s x = Stack x s


type Queue a = (Stack a, Stack a)

move :: Queue a -> Queue a
move q@(_, Empty) = q
move (s1, Stack x s2) = move (Stack x s1, s2)

pushQ :: Queue a -> a -> Queue a
pushQ (s1, s2) x = (s1, push s2 x)

popQ :: Queue a -> Maybe (Queue a, a)
popQ (Empty, Empty) = Nothing
popQ q@(s1, s2) = case pop s1 of
                  Nothing -> popQ $ move q
                  Just (a, b) -> Just ((a, s2), b)

{-- 4 Group for Sum Product --}

class Monoid a => Group a where
  rev :: a -> a

instance Num a => Group (Sum a) where
  rev a = mappend mempty (-a)

instance Fractional a => Group (Product a) where
  rev (Product a) = Product $ recip a
