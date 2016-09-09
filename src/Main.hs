{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where


import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Snowflake
import qualified Data.ByteString.Char8 as B8
import           Data.Monoid
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import           Data.Int (Int64)
import           System.Random (random, newStdGen)
import           Control.Monad.Trans (liftIO)
import           Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar, readMVar)


type Pool = Map.Map Int64 (MVar IdWorker)

workerIds = [0..2^(_workerIdBits defaultConf)-1]

main :: IO ()
main = do
  pool <- Map.fromList . zip workerIds <$> mapM (\x -> newMVar $ IdWorker 0 x 0 0 defaultConf) workerIds
  quickHttpServe $ site pool

site :: Pool -> Snap ()
site pool = ifTop (writeBS "Hello Snowflake") <|> route [ ("get", getHandler pool) ]

snapshot :: Pool -> IO [(Int64, IdWorker)]
snapshot = mapM snapshot' . Map.toList
  where snapshot' (idx, mwk) = do
          wk <- readMVar mwk
          return (idx, wk)

takeIdWorker :: Pool -> Int64 -> IO IdWorker
takeIdWorker pool idx = takeMVar . fromJust . Map.lookup idx $ pool

putIdWorker :: Pool -> Int64 -> IdWorker -> IO ()
putIdWorker pool idx worker = let mwk = fromJust . Map.lookup idx $ pool in putMVar mwk worker

getHandler :: Pool -> Snap ()
getHandler pool = do
  poolS <- liftIO $ snapshot pool
  writeBS $ B8.pack . (<> "\n") . show $ poolS

  n <- (fst . fromJust . B8.readInt . head . fromJust . Map.lookup "n" . rqParams) <$> getRequest
  writeBS $ B8.pack . (<> "\n") . show $ n

  (seed :: Int64) <- liftIO $ ((`mod` 2^(_workerIdBits defaultConf)) . abs . fst . random) <$> newStdGen
  writeBS $ B8.pack . (<> "\n") . show $ seed

  worker <- liftIO $ takeIdWorker pool seed
  writeBS $ B8.pack . (<> "\n") . show $ worker

  (ids, newWorker) <- liftIO $ nexts worker n
  writeBS $ B8.pack . (<> "\n") . show $ ids
  writeBS $ B8.pack . (<> "\n") . show $ newWorker

  liftIO $ putIdWorker pool seed newWorker

  poolS <- liftIO $ snapshot pool
  writeBS $ B8.pack . (<> "\n") . show $ poolS


