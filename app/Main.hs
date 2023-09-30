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

type SweepTheLegAPI = "getSchedule" :> Get '[JSON] Schedule

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

testSchedule = [Schedule "hello" "now" "johan" "https://bild" "https://spotify-link"]

sweepTheLegAPI :: Proxy SweepTheLegAPI
sweepTheLegAPI = Proxy

testServer :: Server SweepTheLegAPI
testServer = return testSchedule

sweepTheBackend :: Application
sweepTheBackend = serve sweepTheLegAPI testServer

main :: IO ()
main = do 
  let configFile = "config.json"
  result <- readConfig configFile
  case result of
    Left err -> putStrLn ("Invalid configuration: " ++ err)
    Right config -> do  let port = httpserver_port config
                        putStrLn $ "Successfully loaded '" ++ configFile ++ "'."
                        putStrLn $ "Starting HTTP server on port " ++ show port ++ "."
                        run port sweepTheBackend
                        return ()

