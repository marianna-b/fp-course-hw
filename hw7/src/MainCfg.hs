{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Control.Exception  as E
import           Control.Monad      (forever)
import qualified Data.IORef         as IORef
import           Data.List          (null)
import qualified Data.Map           as M
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import qualified System.Environment as SE
import           System.Exit        (exitFailure, exitSuccess)
import qualified System.IO          as SIO

processInput :: String -> T.Text -> IORef.IORef (M.Map T.Text T.Text) -> IO ()
processInput _ "A" _ = do
  T.putStrLn "Stopping cfg-creator"
  exitFailure
processInput file "W" config = do
  cfg <- IORef.readIORef config
  T.putStrLn $ "Writing config to file " `T.append` T.pack file
  E.catch (T.writeFile file $ toStringCfg cfg)
      (\e -> do let err = show (e :: E.IOException)
                T.hPutStrLn SIO.stderr ("Warning: " `T.append` T.pack err)
                return ())
  exitSuccess
processInput  _ s config
  | T.take 2 s == "B " = do
      let arg = T.drop 2 s
      cfg <- IORef.readIORef config
      let valid = M.member arg cfg
      if valid then do
          let prev = cfg M.! arg
          T.putStrLn $ "Enter new value for " `T.append` arg `T.append` ". Previous value: " `T.append` prev
          val <- T.getLine
          IORef.writeIORef config (M.insert arg val cfg)
        else
          T.putStrLn "Property not found in config and can't be modified"
  | otherwise = do
      cfg <- IORef.readIORef config
      let (arg, val) = T.breakOn (T.singleton '=') s
      if T.null arg || T.length val < 2 then
        T.putStrLn "Empty property or value"
      else
        if M.member arg cfg then
          T.putStrLn "Property already exists use modification command"
        else
          IORef.writeIORef config (M.insert arg (T.tail val) cfg)

parseCfg :: T.Text -> M.Map T.Text T.Text
parseCfg s = parseCfgList M.empty $ T.split (=='\n') s
  where parseCfgList m ("":xs) = parseCfgList m xs
        parseCfgList m [] = m
        parseCfgList m (x:xs) = parseCfgList newM xs
          where (arg, val) = T.breakOn (T.singleton '=') x
                newM = if T.null arg || T.length val < 2 then m else M.insert arg (T.tail val) m

toStringCfg :: M.Map T.Text T.Text -> T.Text
toStringCfg m = T.intercalate "\n" $ map (\(a, b) -> a `T.append` "=" `T.append` b) $ M.toList m

main :: IO ()
main = do
  args <- SE.getArgs
  if null args then do
    T.putStrLn "File name not specified"
    return ()
  else do
    let file = head args
    cfgText <- E.catch (T.readFile file)
        ((\_ -> return  "") :: E.IOException -> IO T.Text)
    config <- IORef.newIORef $ parseCfg cfgText
    forever $ do
      T.putStrLn "Input property and value:"
      s <- T.getLine
      processInput file (T.strip s) config
