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

type ScheduleAPI = "getSchedule" :> Get '[JSON] [Schedule]

data Schedule = Schedule
  { name :: String 
  , time :: String
  , dj :: String
  , imageLink :: String
  , playlistLink :: String
  } deriving (Show, Eq, Generic)

instance ToJSON Schedule

testSchedules = [Schedule "hello" "now" "johan" "https://bild" "https://spotify-link"]

scheduleAPI :: Proxy ScheduleAPI
scheduleAPI = Proxy

testServer :: Server ScheduleAPI
testServer = return testSchedules

sweepTheBackend :: Application
sweepTheBackend = serve scheduleAPI testServer

main :: IO ()
main = run 1337 sweepTheBackend

