{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS -Wall #-}
module Main where

import           Data.Map (fromList, fold, (!), insert, Map)
import qualified System.Random as R
import qualified Control.Monad.Random as R
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent as C
import           Control.Parallel.Strategies(runEval, rpar)
import           Control.Monad(replicateM)
import           Data.Foldable(traverse_)
import           Control.Lens.TH(makeLenses)
import           Control.Lens ((%~), (&), (.~), (^.), _1)
import qualified Control.Concurrent.ThreadManager as TM

type MagicGenerator = R.StdGen
data Magician = Magician { _attack :: Int
                         , _shield :: Int
                         , _speed :: Int
                         , _instability :: Int
                         , _idx :: Int
                         , _gens :: [MagicGenerator]
                         }
makeLenses ''Magician

genParameters :: (R.RandomGen g) => R.Rand g (Int -> [MagicGenerator] -> Magician)
genParameters = Magician <$> r (1, 200) <*> r (1, 200) <*> r (1, 20) <*> r (10000000, 20000000)
  where r = R.getRandomR

createMagician :: Int -> IO Magician
createMagician i = do
  rand <- R.randomIO
  let g = R.mkStdGen rand
  let (m, r) = R.runRand genParameters g
  return $ m i [r]

pmap :: (a -> b) -> [a] -> [b]
pmap _ [] = []
pmap f (x:xs) = runEval $ do
  x' <- rpar (f x)
  xs' <- rpar (pmap f xs)
  return $ (x':xs')

getInstableRandom :: Int -> Int -> MagicGenerator -> (Int, MagicGenerator)
getInstableRandom up times g = (R.runRand (replicateM times $ R.getRandomR (1, up)) g) & _1 %~ head

attackGen :: Int -> Magician -> (Int, Int, Magician)
attackGen n m = (maximum $ map fst result, target, m & gens .~ newGens')
  where result = pmap (getInstableRandom (m^.attack) (m^.instability)) (m^.gens)
        newGens = map snd result
        (target, newHead) = R.runRand (R.getRandomR (1, n)) $ head newGens
        newGens' = newHead:(tail newGens)

applyAttack :: Int -> Int -> Int -> Map Int Magician -> IO (Map Int Magician, Maybe Magician)
applyAttack mid attackLvl tid stats = do
  let enemy = stats ! tid
      me    = stats ! mid
      count = fold (\x res -> if x^.shield > 0 then res + 1 else res) (0::Int) stats
  if count == 1 then return (stats, Nothing) else do
    if me^.shield == 0 || (enemy^.shield) == 0 then return (stats, Just me) else do
      if enemy^.shield <= attackLvl then do
        let newMe = me & gens %~ (++ enemy^.gens)
        return (insert tid (enemy & shield .~ 0) $ insert mid newMe stats, Just newMe)
      else
        return (insert tid (enemy & shield %~ (\x -> x - attackLvl)) stats, Just me)

fight :: Int -> MVar.MVar (Map Int Magician) -> Magician -> IO ()
fight n stats magician = do
  let (attackLvl, target, m) = attackGen n magician
  C.threadDelay $ 1000 * m^.instability
  updMag <- MVar.modifyMVar stats (applyAttack (m^.idx) attackLvl target)
  case updMag of
    Nothing -> return ()
    Just mag -> if mag^.shield > 0 then fight n stats mag else return ()

main :: IO ()
main = do
  line <- getLine
  let n = read line
  magicians <- traverse createMagician [1 .. n - 1]
  stats <- MVar.newMVar $ fromList $ zip [1 .. n - 1] magicians
  manager <- TM.make
  traverse_ ((TM.fork manager) . fight n stats) magicians
  TM.waitForAll manager
