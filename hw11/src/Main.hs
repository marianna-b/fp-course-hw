{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS -Wall #-}
module Main where


import qualified Control.Concurrent               as C
import qualified Control.Concurrent.MVar          as MVar
import qualified Control.Concurrent.ThreadManager as TM
import           Control.Exception                (bracket_)
import           Control.Lens                     ((%~), (&), (.~), (^.), _1)
import           Control.Lens.TH                  (makeLenses)
import           Control.Monad                    (forM_, when)
import qualified Control.Monad.Random             as R
import           Control.Parallel.Strategies      (rpar, runEval)
import qualified Data.Text                        as T (pack)
import           Data.Vector                      (Vector, foldr', fromList, toList, (!),
                                                   (//))
import           System.Wlog                      (initLoggingFromYaml, logError, logInfo,
                                                   logNotice, releaseAllHandlers,
                                                   usingLoggerName)

type MagicGenerator = R.StdGen
data Magician = Magician { _attack      :: Int
                         , _shield      :: Int
                         , _speed       :: Int
                         , _instability :: Int
                         , _idx         :: Int
                         , _gens        :: [MagicGenerator]
                         }
  deriving Show
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
  return (x':xs')

getInstable :: Int -> Int -> MagicGenerator -> (Int, MagicGenerator)
getInstable up 1 g = R.randomR (1, up) g
getInstable up times g = getInstable up (times - 1) r
  where (_, r) = R.randomR (1, up) g

getTarget :: Int -> Int -> MagicGenerator -> (Int, MagicGenerator)
getTarget n i g = R.runRand (R.getRandomR (0, n - 2)) g & _1 %~ process
  where process target = if target < i then target else target + 1

attackGen :: Int -> Magician -> (Int, Int, Magician)
attackGen n m = (maximum $ map fst result, t, m & gens .~ g:tail (map snd result))
  where result = pmap (getInstable (m^.attack) (m^.instability)) $ m^.gens
        (t, g) = getTarget n (m^.idx) $ snd $ head result

applyAttack :: Magician -> Int -> Int -> Vector Magician -> IO (Vector Magician, Maybe Magician)
applyAttack m attackLvl tid stats
  | me^.shield == 0 = do
      usingLoggerName "fight" $ logError $ T.pack $ "Someone already defeated attacker: " ++ "magician" ++ show mid
      return (stats // [(mid, me)], Nothing)
  | count == 1 = do
    usingLoggerName "fight" $ logError $ T.pack $ "magician" ++ show mid ++ " is the last one"
    return (stats, Nothing)
  | enemy^.shield == 0 = do
      usingLoggerName "fight" $ logError $ T.pack $ "Enemy " ++ "magician" ++ show tid ++ " is already defeated"
      return (stats // [(mid, me)], Just me)
  | enemy^.shield <= attackLvl = do
      let newMe = me & gens %~ (++ enemy^.gens)
      usingLoggerName "fight" $ logNotice $ T.pack $ "magician" ++ show tid ++ " is defeated by " ++ "magician" ++ show mid
      return (stats // [(tid, enemy & shield .~ 0), (mid, newMe)], Just newMe)
  | otherwise = do
      usingLoggerName "fight" $ logNotice $ T.pack $ "magician" ++ show tid ++ " is hit by " ++ "magician" ++ show mid ++ " with " ++ show attackLvl
      return (stats // [(tid, enemy & shield %~ (\x -> x - attackLvl)), (mid, me)], Just me)
  where enemy = stats ! tid
        mid   = m^.idx
        me = (stats ! mid) & gens .~ (m^.gens)
        count = foldr' (\x res -> if x^.shield > 0 then res + 1 else res) (0::Int) stats

fight :: Int -> MVar.MVar (Vector Magician) -> Magician -> IO ()
fight n stats magician = do
  let (attackLvl, target, m) = attackGen n magician
  usingLoggerName "fight" $ do
    logInfo $ T.pack $ "magician" ++ show (m^.idx) ++ " generated attack info: attack force " ++ show attackLvl ++ " target: " ++ show target
    logInfo $ T.pack $ "magician" ++ show (m^.idx) ++ " waiting for " ++ show (m^.speed) ++ " seconds"
  C.threadDelay $ 1000000 * m^.speed
  usingLoggerName "fight" $ logInfo $ T.pack $ "magician" ++ show (m^.idx) ++ " is going to attack " ++ "magician" ++ show target
  Just x <- MVar.modifyMVar stats (applyAttack m attackLvl target)
  when (x^.shield > 0) $ fight n stats x

runThreads :: IO ()
runThreads = do
  line <- getLine
  let n = read line
  magicians <- traverse createMagician [0 .. n - 1]
  stats <- MVar.newEmptyMVar
  MVar.putMVar stats $ fromList magicians
  manager <- TM.make
  forM_ magicians $ \m -> (TM.fork manager . fight n stats) m
  TM.waitForAll manager
  z <- MVar.readMVar stats
  let winner = head $ filter (\x -> x^.shield /= 0) (toList z)
  putStrLn $ "Winner " ++ show (winner^.idx) ++ " shield: " ++ show (winner^.shield) ++ ", amount of generators: "++ show (length (winner^.gens))

main :: IO ()
main = bracket_
  (initLoggingFromYaml "logger-config.yaml" $ Just "logs")
  releaseAllHandlers
  runThreads
