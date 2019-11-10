{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Monad (forever)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Concurrent (threadDelay)
import Data.Binary
import Data.Typeable
import GHC.Generics

{-|
ping pong implemented with dedicated Messages
-}

data Protocol = Ping ProcessId
             | Pong
  deriving (Typeable, Generic, Show)

instance Binary Protocol

ping :: Process ()
ping = forever $ do
  pingPid <- getSelfPid
  pongPid <- whereis "pong"
  
  case pongPid of
    Just actualPid -> do
      send actualPid (Ping pingPid)
      liftIO $ putStrLn "ping sent"
      Pong <- expect
      liftIO $ putStrLn "pong received"
      liftIO $ threadDelay 1000000  
    Nothing ->    do
      liftIO $ putStrLn "pong not ready" 
      liftIO $ threadDelay 1000
   

  
pong :: Process ()
pong = forever $ do
  --myPid <- getSelfPid
  --register "pong" myPid
  Ping pid <- expect
  liftIO $ putStrLn "ping received"
  send pid Pong
  liftIO $ putStrLn "pong sent"
  liftIO $ threadDelay 1000000


main :: IO ()
main = do
  Right t <- createTransport "127.0.0.1" "8080"
			    defaultTCPParameters

  node <- newLocalNode t initRemoteTable
  runProcess node $ do
    pidPong <- spawnLocal pong
    register "pong" pidPong
    
    pidPing <- spawnLocal ping
    register "ping" pidPing
    forever $ liftIO $ threadDelay 1000 
