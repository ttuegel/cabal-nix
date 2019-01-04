{-# LANGUAGE StrictData #-}

module Src where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson ((.:), (.=))
import Data.Data (Data)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map

import Hash

data Src =
    Src
        { urls :: [Text]
        , hash :: Hash  -- ^ Source tarball hash
        }
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance FromJSON Src where
    parseJSON =
        Aeson.withObject "package hashes" parseSrc
      where
        parseSrc obj =
          do
            urls <- obj .: "package-locations"
            hashes <- obj .: "package-hashes"
            case Map.lookup sha256 hashes of
              Nothing -> fail "Missing SHA256 hash"
              Just hash -> return Src { urls, hash }
          where
            sha256 :: Text = "SHA256"

instance ToJSON Src where
    toJSON Src { urls, hash } =
        Aeson.object
            [ "urls" .= Aeson.toJSON urls
            , "hash" .= Aeson.toJSON hash
            ]

readSrc :: FilePath -> IO Src
readSrc file =
  do
    decoded <- Aeson.eitherDecodeFileStrict file
    case decoded of
      Left err ->
          (error . unlines) ["Failed to decode package hashes file:", err]
      Right src ->
          return src
