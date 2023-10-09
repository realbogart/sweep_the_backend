{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module SweepConfig where

import GHC.Generics
import Data.Aeson
import Data.ByteString.Lazy.Char8 qualified as BS
import Control.Exception
import Data.Text qualified as T

data SweepConfig = Config
  { websocket_server_ip :: T.Text
  , httpserver_port :: Int
  , websocket_port :: Int
  , stream_ip :: T.Text
  , password :: T.Text
  , default_background :: T.Text
  } deriving (Show, Generic)

instance ToJSON SweepConfig
instance FromJSON SweepConfig

readConfig :: FilePath -> IO (Either String SweepConfig)
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

