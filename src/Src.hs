module Src where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson ((.:), (.=))
import Data.Data (Data)
import Data.Foldable
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

import Hash

data Src =
    Src
        { urls :: [String]
        , hash :: Hash  -- ^ Source tarball hash
        }
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance FromJSON Src where
    parseJSON =
        Aeson.withObject "package hashes" parseSrc
      where
        parseSrc obj =
          do
            hash <- obj .: "package-hashes" >>= getSha256
            urls <- obj .: "package-locations" >>= getUrls
            return Src { urls, hash }
        getSha256 = Aeson.withObject "hashes" (\obj -> obj .: "SHA256")
        getUrls = Aeson.withArray "URLs" (traverse getUrl . toList)
        getUrl = Aeson.withText "URL" (return . Text.unpack)

instance ToJSON Src where
    toJSON Src { urls, hash } =
        Aeson.object
            [ "urls" .= Aeson.toJSON urls
            , "hash" .= Aeson.toJSON hash
            ]

getSrc :: FilePath -> IO Src
getSrc file =
  do
    decoded <- Aeson.eitherDecodeFileStrict file
    case decoded of
      Left err ->
          (error . unlines) ["Failed to decode package hashes file:", err]
      Right src ->
          return src
