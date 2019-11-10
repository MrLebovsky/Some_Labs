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

data Protocol = Ignition | Inclusion | Request | Answer | Moment | Impact | WoundUp
  deriving (Typeable, Generic, Show)

instance Binary Protocol

benzine :: Process ()
benzine = forever $ do
  benzinePid <- getSelfPid
  
  liftIO $ putStrLn "Benzine: waiting for ignition!"
  Ignition <- expect
  liftIO $ putStrLn "Benzine: ignition received"
  liftIO $ threadDelay 1000000 
  
  motorPid <- whereis "motor"
  case motorPid of
    Just actualPid -> do    
      send actualPid Impact
      liftIO $ putStrLn "Benzine: send impact to motor"
      liftIO $ threadDelay 1000000  
    Nothing ->    do
      liftIO $ putStrLn "motorPid not ready" 
      liftIO $ threadDelay 1000
   

ignition :: Process()
ignition = forever $ do
  liftIO $ putStrLn "Ignition: waiting for inclusion!"
  Inclusion <- expect
  liftIO $ putStrLn "Ignition: inclusion received!"
  liftIO $ threadDelay 1000000 	
  
  benzPid <- whereis "benzine"
  case benzPid of
    Just actualPid -> do    
      send actualPid Ignition
      liftIO $ putStrLn "Ignition: send ignition to benzine"
      liftIO $ threadDelay 1000000  
    Nothing ->    do
      liftIO $ putStrLn "benzinePid not ready" 
      liftIO $ threadDelay 1000


motor :: Process()
motor = forever $ do
  liftIO $ putStrLn "Motor: waiting for impact. I'm a bad guy!"
  Impact <- expect
  liftIO $ putStrLn "Motor: got impact, wound up and working at idle!"
  liftIO $ threadDelay 1000000
  
  driverPid <- whereis "driver"
  case driverPid of
    Just actualPid -> do    
      send actualPid WoundUp
      liftIO $ putStrLn "Motor: send WoundUp to Driver"
      liftIO $ threadDelay 1000000

      liftIO $ putStrLn "Motor: waiting for something..."
      Request <- expect	
      
      liftIO $ putStrLn "Motor: Got the request. Sending answer for driver!"
      send actualPid Answer
      liftIO $ threadDelay 1000000


    Nothing ->    do
      liftIO $ putStrLn "driverPid not ready" 
      liftIO $ threadDelay 1000  
  

driver :: Process()
driver = forever $ do
  ignitionPid <- whereis "ignition"
  case ignitionPid of
    Just actualPid -> do    
      send actualPid Inclusion
      liftIO $ putStrLn "Driver: turning on ignition!"
      liftIO $ threadDelay 1000000
      
      liftIO $ putStrLn "Driver: waiting motor!"
      WoundUp <- expect
      liftIO $ threadDelay 1000000

      motorPid <- whereis "motor"
      case motorPid of
        Just actualPid -> do
          liftIO $ putStrLn "Driver: send request to motor"
          send actualPid Request
          liftIO $ threadDelay 1000000
          liftIO $ putStrLn "Driver: waiting for answer from motor!"
          Answer <- expect
          
          liftIO $ putStrLn "Driver: turn on the transmission!"
          transPid <- whereis "transmission"
          case transPid of
            Just actualPid -> do
              send actualPid Inclusion
              liftIO $ threadDelay 1000000
            Nothing -> do
              liftIO $ putStrLn "transPid not ready"
              liftIO $ threadDelay 1000

          liftIO $ threadDelay 1000000

        Nothing -> do
          liftIO $ putStrLn "motorPid not ready" 
          liftIO $ threadDelay 1000

    Nothing ->    do
      liftIO $ putStrLn "ignitionPid not ready" 
      liftIO $ threadDelay 1000

transmission :: Process()
transmission = forever $ do
  liftIO $ putStrLn "Transmission: waiting for inclusion!"
  Inclusion <- expect
  liftIO $ putStrLn "Transmissson: got the inclusion, sending moment to Wheels!"
  liftIO $ threadDelay 1000000 
  
  wheelsPid <- whereis "wheels"
  case wheelsPid of
    Just actualPid -> do    
      send actualPid Moment
      liftIO $ threadDelay 1000000  
    Nothing ->    do
      liftIO $ putStrLn "wheelsPid not ready" 
      liftIO $ threadDelay 1000

wheels :: Process()
wheels = forever $ do
  liftIO $ putStrLn "Wheels: waiting for a moment!"
  Moment <- expect
  liftIO $ putStrLn "Wheels: start spinning, mission competed!!!!!!!!!!!!!!!!!!!!!!!!"
  liftIO $ threadDelay 1000000

  
main :: IO ()
main = do
  Right t <- createTransport "127.0.0.1" "8080"
			    defaultTCPParameters

  node <- newLocalNode t initRemoteTable
  runProcess node $ do
    pidIgnition <- spawnLocal ignition
    register "ignition" pidIgnition
    
    pidBenzine <- spawnLocal benzine
    register "benzine" pidBenzine

    pidMotor <- spawnLocal motor
    register "motor" pidMotor
  
    pidDriver <- spawnLocal driver
    register "driver" pidDriver
    
    pidTransm <- spawnLocal transmission
    register "transmission" pidTransm

    pidWheels <- spawnLocal wheels
    register "wheels" pidWheels

    forever $ liftIO $ threadDelay 1000 
