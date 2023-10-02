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

type Client = (T.Text, WS.Connection)
type ServerState = [Client]

addClient :: Client -> ServerState -> ServerState
addClient c state = c : state

removeClient :: Client -> ServerState -> ServerState
removeClient c = filter ((== fst c) . fst) 

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

broadcast :: T.Text -> ServerState -> IO ()
broadcast message clients = do
  putStrLn $ "Broadcasting to " ++ show (length clients) ++ " clients:"
  TIO.putStrLn message
  forM_ clients $ \(_, conn) -> WS.sendTextData conn message

talk :: Client -> MVar ServerState -> IO ()
talk (user, conn) state = forever $ do
  msg <- WS.receiveData conn
  readMVar state >>= broadcast msg

wsApplication :: MVar ServerState -> WS.ServerApp
wsApplication state pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ do
    msg :: T.Text <- WS.receiveData conn
    clients <- readMVar state
    case msg of
      -- _ | any ($ fst client)  [T.null, T.any isSpace] ->
      --       WS.sendTextData conn ("Name cannot contain whitespace, and cannot be empty" :: T.Text)
        -- | clientExists client clients ->
        --     WS.sendTextData conn ("Client already exists" :: T.Text)
      _ | otherwise -> 
            flip CE.finally disconnect $ do
              modifyMVar_ state $ \s -> return $ addClient client s
                -- let s' = addClient client s
                -- WS.sendTextData conn $ "Welcome! Users: " <> T.intercalate ", " (map fst s)
                -- broadcast (fst client <> " joined") s'
                -- return s'
              talk client state
        where client = ("none", conn)
              disconnect = do
                modifyMVar_ state $ \s -> return $ removeClient client s
                --   let s' = removeClient client s in return (s', s')
                -- return ()
                -- broadcast (fst client <> " disconnected") s

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
  return $ schedule sweepState

setSchedule :: MVar SweepState -> Schedule -> Handler NoContent
setSchedule state newSchedule = do
  liftIO $ modifyMVar_ state (\oldState -> return oldState {schedule = newSchedule}) 
  return NoContent
  
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
                        sweepState <- newMVar $ SweepState{schedule = testSchedule, currentSlot = - 1}
                        wsState <- newMVar []
                        servantAsync <- A.async $ run http_port (sweepTheApplication sweepState config)
                        websocketAsync <- A.async $ WS.runServer "127.0.0.1" ws_port (wsApplication wsState)
                        A.waitBoth servantAsync websocketAsync
                        return ()

