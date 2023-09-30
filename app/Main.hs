{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import SweepConfig

import Servant
import GHC.Generics
import Network.Wai.Handler.Warp
import Data.Aeson
import Control.Concurrent.MVar
import Control.Monad.IO.Class (MonadIO(liftIO))

data Slot = Schedule
  { name :: String 
  , time :: String
  , dj :: String
  , imageLink :: String
  , playlistLink :: String
  } deriving (Show, Eq, Generic)

type Schedule = [Slot]

instance ToJSON Slot
instance FromJSON Slot

type SweepTheAPI =  "getSchedule" :> Get '[JSON] Schedule :<|>
                    "updateSchedule" :> ReqBody '[JSON] Schedule :> Post '[JSON] NoContent

data SweepState = SweepState
  { schedule :: Schedule
  , currentSlot :: Int
  } deriving (Show)

testSchedule :: Schedule
testSchedule = [Schedule "hello" "now" "johan" "https://bild" "https://spotify-link"]

sweepTheAPI :: Proxy SweepTheAPI
sweepTheAPI = Proxy

getSchedule :: MVar SweepState -> Handler Schedule
getSchedule state = do 
  sweepState <- liftIO (readMVar state)
  return $ schedule sweepState

setSchedule :: MVar SweepState -> Schedule -> Handler NoContent
setSchedule state newSchedule = do
  liftIO $ modifyMVar_ state (\oldState -> return oldState {schedule = newSchedule}) 
  return NoContent
  
sweepTheServer :: MVar SweepState -> Server SweepTheAPI
sweepTheServer state = getSchedule state :<|> setSchedule state

sweepTheApplication :: MVar SweepState -> Application
sweepTheApplication state = serve sweepTheAPI (sweepTheServer state)

main :: IO ()
main = do 
  let configFile = "config.json"
  result <- readConfig configFile
  case result of
    Left err -> putStrLn ("Invalid configuration: " ++ err)
    Right config -> do  let port = httpserver_port config
                        putStrLn $ "Successfully loaded '" ++ configFile ++ "'."
                        putStrLn $ "Starting HTTP server on port " ++ show port ++ "."
                        sweepState <- newMVar $ SweepState{schedule = testSchedule, currentSlot = - 1}
                        run port (sweepTheApplication sweepState)
                        return ()

