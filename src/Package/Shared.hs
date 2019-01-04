{-# LANGUAGE StrictData #-}

module Package.Shared
    ( Package (..)
    , fromGenericPackageDescription
    , fromAllCabalHashes
    ) where

import Data.Aeson (ToJSON)
import Data.Aeson.Types ((.=))
import Data.Data (Data)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Distribution.License (licenseToSPDX)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.SPDX.License (License)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (..))
import Distribution.Types.PackageDescription (PackageDescription (..))
import Distribution.Types.PackageId (PackageIdentifier (..))

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Distribution.Verbosity as Verbosity

import Hash (Hash)
import Orphans ()
import Revision
import Src (Src)

import qualified Hash
import qualified Src

data Package =
    Package
        { package :: PackageIdentifier
        , revision :: Maybe Revision
        , src :: Src
        , license :: License
        , homepage :: Text
        , synopsis :: Text
        }
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance ToJSON Package where
    toJSON pkg =
        Aeson.object
            [ "pname" .= pkgName
            , "version" .= pkgVersion
            , "revision" .= revision
            , "src" .= src
            , "license" .= license
            , "homepage" .= homepage
            , "synopsis" .= synopsis
            ]
      where
        PackageIdentifier { pkgName, pkgVersion } = package
          where
            Package { package } = pkg
        Package { revision, src, license, homepage, synopsis } = pkg

    toEncoding pkg =
        (Aeson.pairs . mconcat)
            [ "pname" .= pkgName
            , "version" .= pkgVersion
            , "revision" .= revision
            , "src" .= src
            , "license" .= license
            , "homepage" .= homepage
            , "synopsis" .= synopsis
            ]
      where
        PackageIdentifier { pkgName, pkgVersion } = package
          where
            Package { package } = pkg
        Package { revision, src, license, homepage, synopsis } = pkg

fromPackageDescription
    :: Hash  -- ^ Hash of the Cabal package description
    -> Src
    -> PackageDescription
    -> Package
fromPackageDescription cabalHash src pkgDesc =
    Package
        { package
        , revision
        , src
        , license
        , homepage
        , synopsis
        }
  where
    PackageDescription { package } = pkgDesc
    homepage = Text.pack homepage'
      where
        PackageDescription { homepage = homepage' } = pkgDesc
    synopsis = Text.pack synopsis'
      where
        PackageDescription { synopsis = synopsis' } = pkgDesc
    revision =
      case lookup "x-revision" customFieldsPD of
        Nothing ->
            Nothing
        Just n ->
            Just Revision { revision = read n, hash = cabalHash }
      where
        PackageDescription { customFieldsPD } = pkgDesc
    license = either id licenseToSPDX licenseRaw
      where
        PackageDescription { licenseRaw } = pkgDesc

fromGenericPackageDescription
    :: Hash  -- ^ Hash of the Cabal package description
    -> Src
    -> GenericPackageDescription
    -> Package
fromGenericPackageDescription cabalHash src gPkgDesc =
    fromPackageDescription cabalHash src pkgDesc
  where
    GenericPackageDescription { packageDescription = pkgDesc } = gPkgDesc

fromAllCabalHashes
    :: FilePath  -- ^ Cabal package description file
    -> FilePath  -- ^ all-cabal-hashes file
    -> IO Package
fromAllCabalHashes cabal json =
    fromGenericPackageDescription
        <$> Hash.nixHash cabal
        <*> Src.readSrc json
        <*> readGenericPackageDescription Verbosity.silent cabal
