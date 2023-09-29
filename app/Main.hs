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

readConfig :: FilePath -> IO (Either String Config)
readConfig fileName = do
  result <- try (BS.readFile fileName) :: IO (Either IOException BS.ByteString)
  return $ case result of
            Left ex -> do 
              Left $ "Unable to read config file " ++ show ex 
            Right content -> do 
              let parseResult = eitherDecode content
              case parseResult of
                Left err -> do 
                  Left $ "Could not parse JSON from " ++ show fileName ++ ": " ++ show err
                Right config -> Right config

main :: IO ()
main = do 
  result <- readConfig "config.json"
  case result of
    Left err -> putStrLn ("Invalid configuration: " ++ err)
    Right config -> do  print config
                        run (httpserver_port config) sweepTheBackend
                        return ()

