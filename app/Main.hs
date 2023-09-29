{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Text
import Servant
import Servant.API
import GHC.Generics
import Network.Wai.Handler.Warp
import Data.Aeson
import Data.ByteString.Lazy.Char8 qualified as BS
import Control.Exception

type ScheduleAPI = "getSchedule" :> Get '[JSON] [Schedule]

data Schedule = Schedule
  { name :: String 
  , time :: String
  , dj :: String
  , imageLink :: String
  , playlistLink :: String
  } deriving (Show, Eq, Generic)

data Config = Config
  { websocket_server_ip :: String
  , httpserver_port :: Int
  , websocket_port :: Int
  , stream_ip :: String
  , password :: String
  , default_background :: String
  } deriving (Show, Generic)

instance ToJSON Schedule

instance ToJSON Config
instance FromJSON Config

testSchedules = [Schedule "hello" "now" "johan" "https://bild" "https://spotify-link"]

scheduleAPI :: Proxy ScheduleAPI
scheduleAPI = Proxy

testServer :: Server ScheduleAPI
testServer = return testSchedules

sweepTheBackend :: Application
sweepTheBackend = serve scheduleAPI testServer

readConfig :: FilePath -> IO (Maybe Config)
readConfig fileName = do
  result <- try (BS.readFile fileName) :: IO (Either IOException BS.ByteString)
  case result of
    Left ex -> do 
      putStrLn $ "Unable to read config file " ++ show ex 
      return Nothing
    Right content -> do 
      let parseResult = decode content
      case parseResult of
        Nothing -> do 
          putStrLn $ "Could not parse JSON from " ++ show fileName
          return Nothing
        Just config -> return $ Just config

main :: IO ()
main = do 
  result <- readConfig "config.json"
  case result of
    Nothing -> putStrLn "Invalid configuration"
    Just config -> print config
  run 1337 sweepTheBackend
  return ()

