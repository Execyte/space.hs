module Main (main) where

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async

import Network.QUIC.Simple
import Network.Message

import Data.IORef (newIORef, atomicModifyIORef')

import Server.Simulation

main :: IO ()
main = do
  counter <- newIORef 0
  runServerSimple "127.0.0.1" 2525 \case
    Hello -> do
      putStrLn "Server got Hello"
      n <- atomicModifyIORef' counter \old -> (old + 1, old)
      pure $ Ok n
    Ping -> do
      putStrLn "Got ping"
      pure Pong

