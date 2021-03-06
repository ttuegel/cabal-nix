{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans () where

import Data.Aeson (ToJSON, ToJSONKey)
import Data.Text (Text)
import Distribution.Pretty (Pretty)
import Distribution.System (Platform (..))
import Distribution.Types.GenericPackageDescription (FlagName)
import Distribution.Types.PackageId (PackageIdentifier)
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.UnqualComponentName (UnqualComponentName)
import Distribution.Types.Version (Version)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as Text
import qualified Distribution.Pretty as Pretty
import qualified Distribution.SPDX.License as SPDX

prettyKey :: Pretty a => a -> Text
prettyKey = Text.pack . Pretty.prettyShow

instance ToJSON FlagName where
    toJSON = Aeson.toJSON . prettyKey
    toEncoding = Aeson.toEncoding . prettyKey

instance ToJSONKey FlagName where
    toJSONKey = Aeson.toJSONKeyText prettyKey

instance ToJSON SPDX.License where
    toJSON = Aeson.toJSON . prettyKey
    toEncoding = Aeson.toEncoding . prettyKey

instance ToJSONKey PackageIdentifier where
    toJSONKey = Aeson.toJSONKeyText prettyKey

instance ToJSON PackageIdentifier where
    toJSON = Aeson.toJSON . prettyKey
    toEncoding = Aeson.toEncoding . prettyKey

instance ToJSON PackageName where
    toJSON = Aeson.toJSON . prettyKey
    toEncoding = Aeson.toEncoding . prettyKey

instance ToJSON Platform where
    toJSON = Aeson.toJSON . prettyKey
    toEncoding = Aeson.toEncoding . prettyKey

instance ToJSONKey Platform where
    toJSONKey = Aeson.toJSONKeyText prettyKey

instance ToJSON UnqualComponentName where
    toJSON = Aeson.toJSON . prettyKey
    toEncoding = Aeson.toEncoding . prettyKey

instance ToJSONKey UnqualComponentName where
    toJSONKey = Aeson.toJSONKeyText prettyKey

instance ToJSON Version where
    toJSON = Aeson.toJSON . prettyKey
    toEncoding = Aeson.toEncoding . prettyKey
