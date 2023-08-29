{-# LANGUAGE OverloadedStrings #-}
module Config where

import Prelude hiding (log, readFile)
import Data.Maybe (fromMaybe)
import Data.Aeson
import Control.Applicative
import Data.ByteString.Lazy

import Util

configFileName :: FilePath
configFileName = "vlconfig.json"

data VLConfig = VLConfig
  { root :: FilePath
  , extension :: Extension
  } deriving (Show)

instance FromJSON VLConfig where
  parseJSON (Object v) = VLConfig <$>
        (v .: "root")
    <*> (v .: "extension")

instance ToJSON VLConfig where
  toJSON (VLConfig root extension) = object [
      "root" .= root
    , "extension".= extension
    ]

decodeConfig :: IO (FilePath, Extension)
decodeConfig = do
  content <- readFile configFileName
  let config = fromMaybe (error "Failed to parse vlconfig.json.") $ decode content
  return (root config, extension config)

----------------------

instance PrettyAST VLConfig where
  ppP (VLConfig r e) =
      ppP ("VLConfig {" :: String) <>
      ppP ("root =" :: String) <> ppP r <>
      ppP ("extension =" :: String) <> ppP e <>
      ppP ("}" :: String)