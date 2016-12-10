module Main where

import Lib

main :: IO ()
main = do
  putStrLn "Safe head tests"
  print $ safeHead ([]::[Int])
  print $ safeHead [1, 3, 5]
