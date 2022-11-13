{-# LANGUAGE OverloadedStrings #-}
module Config where

import Prelude hiding (log, readFile)
import Data.Maybe (fromMaybe)
import Data.Aeson
import Control.Applicative
import Data.ByteString.Lazy

import Util

configFileName :: String
configFileName = "vlconfig.json"

data VLConfig = VLConfig
  { root :: FilePath
  , log  :: FilePath
  , extension :: String
  } deriving (Show)

instance FromJSON VLConfig where
  parseJSON (Object v) = VLConfig <$>
        (v .: "root")
    <*> (v .: "log")
    <*> (v .: "extension")

instance ToJSON VLConfig where
  toJSON (VLConfig root log extension) = object [
      "root" .= root
    , "log" .= log
    , "extension".= extension
    ]

decodeConfig :: IO (FilePath, FilePath, String)
decodeConfig = do
  content <- readFile configFileName
  let config = fromMaybe (error "Failed to parse vlconfig.json.") $ decode content
  return (root config, log config, extension config)

----------------------

instance PrettyAST VLConfig where
  ppE (VLConfig r l e) =
      ppE ("VLConfig {" :: String) <>
      ppE ("root =" :: String) <> ppE r <> 
      ppE ("log =" :: String) <> ppE l <>
      ppE ("extension =" :: String) <> ppE e <>
      ppE ("}" :: String)
  ppP (VLConfig r l e) =
      ppP ("VLConfig {" :: String) <>
      ppP ("root =" :: String) <> ppP r <> 
      ppP ("log =" :: String) <> ppP l <>
      ppP ("extension =" :: String) <> ppP e <>
      ppP ("}" :: String)