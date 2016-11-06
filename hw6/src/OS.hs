{-# OPTIONS -Wall #-}
module OS where

import           Control.Monad.Cont (Cont, cont, runCont)

type Input = [String]
type Output = [String]
type EarlyRet r = (World r, ()) -> r
data World r = World Input Output [Process r] (EarlyRet r)
type Process r = World r -> Cont r (World r, ())

type Function a b r = (World r, a) -> Cont r (World r, b)

readLnCPS :: Function () String r
readLnCPS (World (x:xs) ys p ret, _) = return (World xs ys p ret, x)
readLnCPS (World [] ys p ret, _)     = return (World [] ys p ret, "")

reverseCPS :: Function String String r
reverseCPS (w, s) = return (w, reverse s)

writeCPS :: Function String () r
writeCPS (World xs ys p ret, s) = return (World xs (s:ys) p ret, ())

writeLn :: String -> Function () () r
writeLn s (w, ()) = writeCPS (w, s)

yieldCPS :: Function () () r
yieldCPS (World xs ys ps ret, ()) = cont $ \f -> ret (nw f, ())
  where nw f = World xs ys (ps ++ [\w -> cont $ \_ -> f (w, ())]) ret

forkCPS :: Function () Bool r
forkCPS (World xs ys p ret, ()) = cont $ \f -> f (World xs ys (child f:p) ret, True)
  where child f w = cont $ \_ -> f (w, False)

exitCPS :: Function () () r
exitCPS = return

continueFromFork :: Function Bool String r
continueFromFork (w, b)
  | b = cont $ \f -> f (w, "Parent process")
  | otherwise = cont $ \f -> f (w, "Child process")

process1 :: World r -> Cont r (World r, ())
process1 w = readLnCPS (w, ()) >>= reverseCPS >>= writeCPS >>= writeLn "Some text" >>= exitCPS

process2 :: World r -> Cont r (World r, ())
process2 w = readLnCPS (w, ()) >>= reverseCPS >>= writeCPS >>= forkCPS >>= continueFromFork >>= writeCPS >>= writeLn "Wow" >>= exitCPS

process3 :: World r -> Cont r (World r, ())
process3 w = writeLn "Pass control" (w, ()) >>= yieldCPS >>= writeLn "Return control" >>= exitCPS

kernel :: [String] -> [Process [String]] -> [String]
kernel _ [] = []
kernel xs (p:ps) = runCont (p (World xs [] ps ret)) ret
  where ret (World input output procs _, ()) = reverse output ++ kernel input procs

test :: [String]
test = kernel ["Test 1", "Test 2"] [process1, process2]
