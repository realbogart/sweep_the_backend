{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import SweepConfig

import Servant
import GHC.Generics
import Network.Wai.Handler.Warp
import Network.Wai
import Network.HTTP.Types
import Data.Aeson
import Control.Concurrent.MVar
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ByteString.Char8 qualified as BS
import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Text.IO qualified as TIO
import Data.Text.Lazy.IO qualified as TLIO
import System.FilePath.Posix

data Slot = Schedule
  { name :: T.Text 
  , time :: T.Text
  , dj :: T.Text
  , imageLink :: T.Text
  , playlistLink :: T.Text
  } deriving (Show, Eq, Generic)

type Schedule = [Slot]

instance ToJSON Slot
instance FromJSON Slot

type SweepTheAPI =  "getSchedule" :> Get '[JSON] Schedule :<|>
                    "setSchedule" :> ReqBody '[JSON] Schedule :> Post '[JSON] NoContent :<|>
                    Raw

data SweepState = SweepState
  { schedule :: Schedule
  , currentSlot :: Int
  } deriving (Show)

testSchedule :: Schedule
testSchedule = [Schedule "hello" "now" "johan" "https://bild" "https://spotify-link"]

sweepTheAPI :: Proxy SweepTheAPI
sweepTheAPI = Proxy

isHtmlRequest :: Request -> Bool
isHtmlRequest req = ".html" `L.isSuffixOf` BS.unpack (rawPathInfo req)

replacePlaceholders :: SweepConfig -> TL.Text -> TL.Text
replacePlaceholders config = 
  TL.replace "<websocket_server>" (TL.fromStrict $ websocket_server_ip config)

serveModifiedHtml :: SweepConfig -> FilePath -> Application
serveModifiedHtml config filePath req respond = do
    content <- TLIO.readFile filePath
    let modifiedContent = replacePlaceholders config content
    respond $ responseLBS status200 [("Content-Type", "text/html")] (TLE.encodeUtf8 modifiedContent)

htmlMiddleware :: SweepConfig -> Middleware
htmlMiddleware config app req respond =
    if isHtmlRequest req
    then serveModifiedHtml config (joinPath $ map T.unpack $ pathInfo req) req respond
    else app req respond

getSchedule :: MVar SweepState -> Handler Schedule
getSchedule state = do 
  sweepState <- liftIO (readMVar state)
  return $ schedule sweepState

setSchedule :: MVar SweepState -> Schedule -> Handler NoContent
setSchedule state newSchedule = do
  liftIO $ modifyMVar_ state (\oldState -> return oldState {schedule = newSchedule}) 
  return NoContent
  
sweepTheServer :: MVar SweepState -> Server SweepTheAPI
sweepTheServer state = getSchedule state :<|> setSchedule state :<|> serveDirectoryFileServer "static"

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

