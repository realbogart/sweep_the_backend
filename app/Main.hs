{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

import SweepConfig

import Servant
import Servant.API.Header qualified as SH
import Servant.Server.Experimental.Auth as SA
import Servant.Auth.Server
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
import Data.ByteString.Base64 qualified as BS64
import Data.ByteString.Lazy.Char8 qualified as BSL
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
import Data.Maybe (fromMaybe)
import Crypto.Random (getRandomBytes)
import Web.Cookie (SetCookie(..), defaultSetCookie, parseCookies)

type Client = (Int, WS.Connection)

data Slot = Slot
  { name :: T.Text 
  , time :: T.Text
  , dj :: T.Text
  , imageLink :: T.Text
  , playlistLink :: T.Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Schedule = Schedule 
  { slots :: [Slot]
  } deriving (Show, Generic, ToJSON, FromJSON)

data CommandMessage a = CommandMessage
  { command :: T.Text 
  , message :: a
  } deriving (Show, Generic, ToJSON, FromJSON)

data ScheduleMessage = ScheduleMessage
  { activeSchedule :: Schedule
  , activeSlot :: Int
  } deriving (Show, Generic, ToJSON, FromJSON)

data SweepState = SweepState
  { schedule :: Schedule
  , currentActiveSlot :: Int
  , clients :: [Client]
  , currentClientID :: Int
  , jwtSettings :: JWTSettings
  }

data AdminUser = AdminUser
  { username :: T.Text 
  } deriving (Show, Generic, ToJSON, ToJWT, FromJSON, FromJWT)

data AdminCredentials = AdminCredentials
  { password :: T.Text 
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

isValidJWT :: JWTSettings -> BS.ByteString -> Handler AdminUser
isValidJWT jwtSettings jwt = do
  verified <- liftIO $ verifyJWT jwtSettings jwt 
  case verified of
    Nothing -> throwError (err403 { errBody = BS.fromStrict ("Invalid jwt: " <> jwt) })
    Just a -> return a

authHandler :: MVar SweepState -> AuthHandler Request AdminUser
authHandler state = SA.mkAuthHandler handler
  where
    maybeToEither e = maybe (Left e) Right
    throw401 msg = throwError $ err401 { errBody = msg }
    handler req = do 
      s <- liftIO $ readMVar state
      either throw401 (isValidJWT s.jwtSettings) $ do
        cookie <- maybeToEither "Missing cookie header" $ lookup "cookie" $ requestHeaders req
        maybeToEither "Missing JWT in cookie" $ lookup "jwt" $ parseCookies cookie

getJWTSettings :: BS.ByteString -> JWTSettings
getJWTSettings secret = defaultJWTSettings (fromSecret secret)

generateJWTSecret :: IO BS.ByteString
generateJWTSecret = BS64.encode <$> getRandomBytes 64

generateClientId :: MVar SweepState -> IO Int
generateClientId state = modifyMVar state (\s -> return (s{currentClientID = s.currentClientID + 1}, s.currentClientID))

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
  forM_ state.clients $ \(clientID, conn) -> 
    CE.catch  (WS.sendTextData conn message)
              (\e ->  putStrLn $ "Failed to send message to client #" ++ 
                      show clientID ++ ": " ++ show (e :: CE.SomeException))

isFireworks :: T.Text -> Bool
isFireworks = T.isPrefixOf (T.pack "{\"command\":\"spawnFireworks\",")

getScheduleMsg :: MVar SweepState -> IO TL.Text
getScheduleMsg state = do
  s <- readMVar state
  return $ encodeToLazyText $ CommandMessage "setSchedule" (ScheduleMessage s.schedule s.currentActiveSlot)

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

type instance AuthServerData (AuthProtect "cookie-jwt-auth") = AdminUser

type SweepTheAPI =  "getSchedule" :> Get '[JSON] Schedule :<|>
                    "setSchedule" :> ReqBody '[JSON] Schedule :> Post '[JSON] () :<|>
                    "setActiveSlot" :> QueryParam "id" Int :> Get '[JSON] () :<|>
                    "login" :> ReqBody '[JSON] AdminCredentials 
                            :> Post '[JSON] (Headers '[SH.Header "Set-Cookie" SetCookie] T.Text) :<|>
                    "test" :> AuthProtect "cookie-jwt-auth" :> Get '[PlainText] T.Text :<|>
                    -- "admin" :> Auth '[JWT] AdminUser :> Get '[PlainText] T.Text :<|>
                    Raw

sweepTheContext :: MVar SweepState -> Context (AuthHandler Request AdminUser ': '[])
sweepTheContext state = authHandler state :. EmptyContext

testSchedule :: Schedule
testSchedule = Schedule [Slot "Funky Friday" "10:00" "johan" "https://bild" "https://spotify-link"]

sweepTheAPI :: Proxy SweepTheAPI
sweepTheAPI = Proxy

staticPath :: String
staticPath = "static"

type Replacement = (TL.Text, TL.Text)

replacePlaceholders :: SweepConfig -> TL.Text -> TL.Text
replacePlaceholders config content = 
  L.foldl' replace content replacements
    where replacements :: [Replacement]
          replacements =  [ ("<websocket_server", TL.fromStrict config.websocket_server_ip)
                          , ("<stream_ip>", TL.fromStrict config.stream_ip)
                          , ("<password>", TL.fromStrict config.password)]
          replace haystack (needle,replacement) = TL.replace needle replacement haystack

getModifiedHtml :: SweepConfig -> FilePath -> IO TL.Text
getModifiedHtml config filePath = do
  content <- TLIO.readFile (staticPath ++ "/" ++ filePath)
  return $ replacePlaceholders config content

serveModifiedHtml :: SweepConfig -> FilePath -> Application
serveModifiedHtml config filePath req respond = do
  modifiedContent <- getModifiedHtml config filePath
  respond $ responseLBS status200 [("Content-Type", "text/html")] (TLE.encodeUtf8 modifiedContent)

isHtmlRequest :: Request -> Bool
isHtmlRequest req = ".html" `L.isSuffixOf` BS.unpack (rawPathInfo req)

isAdminRequest :: Request -> Bool
isAdminRequest req = "admin.html" `L.isSuffixOf` BS.unpack (rawPathInfo req)

htmlMiddleware :: SweepConfig -> Middleware
htmlMiddleware config app req respond
  | null path = serveModifiedHtml config "index.html" req respond
  -- | isAdminRequest req = serveModifiedHtml config "" 
  | isHtmlRequest req = serveModifiedHtml config (joinPath $ map T.unpack path) req respond
  | otherwise = app req respond
    where path = pathInfo req

getSchedule :: MVar SweepState -> Handler Schedule
getSchedule state = do 
  sweepState <- liftIO (readMVar state)
  return sweepState.schedule

setSchedule :: MVar SweepState -> Schedule -> Handler ()
setSchedule state newSchedule = do
  newState <- liftIO $ modifyMVar state $ \oldState -> do
      let updatedState = oldState {schedule = newSchedule}
      return (updatedState, updatedState) 
  scheduleMsg <- liftIO $ getScheduleMsg state
  liftIO $ broadcast (TL.toStrict scheduleMsg) newState
  return ()

setActiveSlot :: MVar SweepState -> Maybe Int -> Handler ()
setActiveSlot state newActiveSlot = do
  newState <- liftIO $ modifyMVar state $ \oldState -> do
      let updatedState = oldState {currentActiveSlot = fromMaybe (- 1) newActiveSlot}
      return (updatedState, updatedState) 
  scheduleMsg <- liftIO $ getScheduleMsg state
  liftIO $ broadcast (TL.toStrict scheduleMsg) newState
  return ()

loginHandler :: MVar SweepState -> 
                SweepConfig -> 
                AdminCredentials -> 
                Handler (Headers '[SH.Header "Set-Cookie" SetCookie] T.Text)
loginHandler state config creds = do
  liftIO $ putStrLn "Someone is attempting to login as admin."
  s <- liftIO $ readMVar state
  if creds.password == config.password
      then do 
        liftIO $ putStrLn "Admin login successful. Creating JWT"
        jwt <- liftIO $ makeJWT (AdminUser "admin") s.jwtSettings Nothing 
        case jwt of
          Left err -> throwError err500 {errBody = BSL.pack ("Failed to create JWT: " ++ show err)}
          Right token -> do
            let cookie = defaultSetCookie { setCookieName = "jwt"
                                          , setCookieValue = BS.toStrict token
                                          , setCookiePath = Just "/"
                                          , setCookieHttpOnly = True
                                          }
            return $ addHeader cookie "Login successful"
      else throwError err401 { errBody = "Access denied" }

testAuthHandler :: AdminUser -> Handler T.Text
testAuthHandler _ = return $ T.pack "hello admin"

sweepTheServer :: MVar SweepState -> SweepConfig -> Server SweepTheAPI
sweepTheServer state config = getSchedule state :<|> 
                              setSchedule state :<|> 
                              setActiveSlot state :<|>
                              loginHandler state config :<|>
                              testAuthHandler :<|>
                              serveDirectoryFileServer staticPath

sweepTheApplication :: MVar SweepState -> SweepConfig -> Application
sweepTheApplication state config =  htmlMiddleware config $ 
                                    serveWithContext sweepTheAPI (sweepTheContext state) (sweepTheServer state config)

main :: IO ()
main = do 
  let configFile = "config.json"
  result <- readConfig configFile
  case result of
    Left err -> putStrLn ("Invalid configuration: " ++ err)
    Right config -> do  let http_port = config.httpserver_port
                        let ws_port = config.websocket_port
                        putStrLn $ "Successfully loaded '" ++ configFile ++ "'."
                        putStrLn $ "Starting HTTP server on port " ++ show http_port ++ "."
                        putStrLn $ "Starting websocket server on port " ++ show ws_port ++ "."
                        jwtSecret <- generateJWTSecret
                        sweepState <- newMVar $ SweepState{ schedule = testSchedule
                                                          , currentActiveSlot = - 1 
                                                          , clients = []
                                                          , currentClientID = 0
                                                          , jwtSettings = getJWTSettings jwtSecret
                                                          }
                        servantAsync <- A.async $ run http_port (sweepTheApplication sweepState config)
                        websocketAsync <- A.async $ WS.runServer "127.0.0.1" ws_port (wsApplication sweepState)
                        A.waitBoth servantAsync websocketAsync
                        return ()

