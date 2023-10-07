{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

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
import Control.Monad
import Control.Exception qualified as CE
import Data.ByteString.Char8 qualified as BS
import Data.Char
import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Text.IO qualified as TIO
import Data.Text.Lazy.IO qualified as TLIO
import System.FilePath.Posix
import Control.Concurrent.Async qualified as A
import qualified Network.WebSockets as WS
import Data.Aeson.Text (encodeToLazyText)
import Network.WebSockets (PendingConnection)

type Client = (Int, WS.Connection)

data Slot = Slot
  { name :: T.Text 
  , time :: T.Text
  , dj :: T.Text
  , imageLink :: T.Text
  , playlistLink :: T.Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Schedule = Schedule 
  { shows :: [Slot]
  } deriving (Show, Generic, ToJSON, FromJSON)

data CommandMessage a = CommandMessage
  { command :: T.Text 
  , message :: a
  } deriving (Show, Generic, ToJSON, FromJSON)

data SweepState = SweepState
  { schedule :: Schedule
  , currentSlot :: Int
  , clients :: [Client]
  , current_client_id :: Int
  }

generateClientId :: MVar SweepState -> IO Int
generateClientId state = modifyMVar state (\s -> return (s{current_client_id = s.current_client_id + 1}, s.current_client_id))

addClient :: Client -> SweepState -> SweepState
addClient c state = state { clients = c : state.clients}

removeClient :: Client -> SweepState -> SweepState
removeClient c state = state { clients = filter ((/= fst c) . fst) state.clients } 

numClients :: SweepState -> Int
numClients SweepState{clients = c} = length c

broadcast :: T.Text -> SweepState -> IO ()
broadcast message state = do
  putStrLn $ "Broadcasting to " ++ show (numClients state) ++ " clients:"
  TIO.putStrLn message
  forM_ state.clients $ \(_, conn) -> WS.sendTextData conn message

isFireworks :: T.Text -> Bool
isFireworks = T.isPrefixOf (T.pack "{\"command\":\"spawnFireworks\",")

getScheduleMsg :: MVar SweepState -> IO TL.Text
getScheduleMsg state = do
  s <- readMVar state
  return $ encodeToLazyText $ CommandMessage "setSchedule" s.schedule

getMessageFromClient :: Client -> MVar SweepState -> IO ()
getMessageFromClient (user, conn) state = forever $ do
  msg <- WS.receiveData conn
  if isFireworks msg
  then readMVar state >>= broadcast msg
  else putStrLn $ "Not broadcasting message: " ++ T.unpack msg
  
wsApplication :: MVar SweepState -> PendingConnection -> IO ()
wsApplication state pending = do
  conn <- WS.acceptRequest pending
  clientID <- generateClientId state

  WS.withPingThread conn 30 (return ()) $ do
    let client = (clientID, conn)
        disconnect = do modifyMVar_ state $ \s -> return $ removeClient client s
                        putStrLn $ "Client #" ++ show clientID ++ " disconnected."

    putStrLn $ "Client #" ++ show clientID ++ " connected."
    scheduleMsg <- getScheduleMsg state
    WS.sendTextData conn scheduleMsg

    flip CE.finally disconnect $ do
      modifyMVar_ state $ \s -> return $ addClient client s
      getMessageFromClient client state

type SweepTheAPI =  "getSchedule" :> Get '[JSON] Schedule :<|>
                    "setSchedule" :> ReqBody '[JSON] Schedule :> Post '[JSON] () :<|>
                    Raw

testSchedule :: Schedule
testSchedule = Schedule [Slot "hello" "now" "johan" "https://bild" "https://spotify-link"]

sweepTheAPI :: Proxy SweepTheAPI
sweepTheAPI = Proxy

staticPath :: String
staticPath = "static"

isHtmlRequest :: Request -> Bool
isHtmlRequest req = ".html" `L.isSuffixOf` BS.unpack (rawPathInfo req)

type Replacement = (TL.Text, TL.Text)

replacePlaceholders :: SweepConfig -> TL.Text -> TL.Text
replacePlaceholders config content = 
  L.foldl' replace content replacements
    where replacements :: [Replacement]
          replacements =  [ ("<websocket_server", TL.fromStrict $ websocket_server_ip config)
                          , ("<stream_ip>", TL.fromStrict $ stream_ip config)]
          replace haystack (needle,replacement) = TL.replace needle replacement haystack

serveModifiedHtml :: SweepConfig -> FilePath -> Application
serveModifiedHtml config filePath req respond = do
  content <- TLIO.readFile (staticPath ++ "/" ++ filePath)
  let modifiedContent = replacePlaceholders config content
  respond $ responseLBS status200 [("Content-Type", "text/html")] (TLE.encodeUtf8 modifiedContent)

htmlMiddleware :: SweepConfig -> Middleware
htmlMiddleware config app req respond
  | null path = serveModifiedHtml config "index.html" req respond
  | isHtmlRequest req = serveModifiedHtml config (joinPath $ map T.unpack path) req respond
  | otherwise = app req respond
    where path = pathInfo req

getSchedule :: MVar SweepState -> Handler Schedule
getSchedule state = do 
  sweepState <- liftIO (readMVar state)
  return sweepState.schedule

setSchedule :: MVar SweepState -> Schedule -> Handler ()
setSchedule state newSchedule = do
  liftIO $ modifyMVar_ state (\oldState -> return oldState {schedule = newSchedule}) 
  return ()
  
sweepTheServer :: MVar SweepState -> Server SweepTheAPI
sweepTheServer state = getSchedule state :<|> setSchedule state :<|> serveDirectoryFileServer staticPath

sweepTheApplication :: MVar SweepState -> SweepConfig -> Application
sweepTheApplication state config = htmlMiddleware config $ serve sweepTheAPI (sweepTheServer state)

main :: IO ()
main = do 
  let configFile = "config.json"
  result <- readConfig configFile
  case result of
    Left err -> putStrLn ("Invalid configuration: " ++ err)
    Right config -> do  let http_port = httpserver_port config
                        let ws_port = websocket_port config
                        putStrLn $ "Successfully loaded '" ++ configFile ++ "'."
                        putStrLn $ "Starting HTTP server on port " ++ show http_port ++ "."
                        putStrLn $ "Starting websocket server on port " ++ show ws_port ++ "."
                        sweepState <- newMVar $ SweepState{ schedule = testSchedule, 
                                                            currentSlot = - 1, 
                                                            clients = [], 
                                                            current_client_id = 0}
                        servantAsync <- A.async $ run http_port (sweepTheApplication sweepState config)
                        websocketAsync <- A.async $ WS.runServer "127.0.0.1" ws_port (wsApplication sweepState)
                        A.waitBoth servantAsync websocketAsync
                        return ()

