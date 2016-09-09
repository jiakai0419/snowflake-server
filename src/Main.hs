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

_N = 2^(_workerIdBits defaultConf)

initPool :: IO Pool
initPool = Map.fromList . zip workerIds <$> mapM (\x -> newMVar $ IdWorker 0 x 0 0 defaultConf) workerIds
  where workerIds = [0.._N-1]

takeIdWorker :: Pool -> Int64 -> IO IdWorker
takeIdWorker pool idx = takeMVar . fromJust . Map.lookup idx $ pool

putIdWorker :: Pool -> Int64 -> IdWorker -> IO ()
putIdWorker pool idx worker = let mwk = fromJust . Map.lookup idx $ pool in putMVar mwk worker

dispatchAndGen :: Pool -> Int -> IO [Int64]
dispatchAndGen pool n = do
  seed <- ((`mod` _N) . abs . fst . random) <$> newStdGen
  worker <- takeIdWorker pool seed
  (ids, newWorker) <- nexts worker n
  putIdWorker pool seed newWorker
  return ids

main :: IO ()
main = do
  pool <- initPool
  quickHttpServe $ site pool

site :: Pool -> Snap ()
site pool = ifTop (writeBS "Hello Snowflake") <|> route [ ("get", getHandler pool) ]

getHandler :: Pool -> Snap ()
getHandler pool = do
  n <- (fst . fromJust . B8.readInt . head . fromJust . Map.lookup "n" . rqParams) <$> getRequest
  ids <- liftIO $ dispatchAndGen pool n
  writeBS . B8.pack . show $ ids

