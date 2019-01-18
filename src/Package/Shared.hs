{-# LANGUAGE StrictData #-}

module Package.Shared
    ( Package (..)
    , fromGenericPackageDescription
    ) where

import Data.Data (Data)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Distribution.License (licenseToSPDX)
import Distribution.SPDX.License (License)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (..))
import Distribution.Types.PackageDescription (PackageDescription (..))
import Distribution.Types.PackageId (PackageIdentifier (..))
import Nix.Expr (($=))
import Numeric.Natural (Natural)

import qualified Data.Text as Text
import qualified Nix.Expr

import Cabal (Cabal)
import Express (Express)
import Orphans ()
import Src (Src)

import qualified Express

data Package =
    Package
        { package :: PackageIdentifier
        , cabal :: Cabal
        , src :: Src
        , license :: License
        , homepage :: Text
        , synopsis :: Text
        }
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance Express Package where
    express pkg =
        Nix.Expr.mkNonRecSet
            [ "package" $= Express.express package
            , "cabal" $= Express.express cabal
            , "src" $= Express.express src
            , "license" $= Express.express license
            , "homepage" $= Express.express homepage
            , "synopsis" $= Express.express synopsis
            ]
      where
        Package { package, cabal, src, license, homepage, synopsis } = pkg

fromPackageDescription
    :: (Natural -> Cabal)
    -> Src
    -> PackageDescription
    -> Package
fromPackageDescription cabalAt src pkgDesc =
    Package
        { package
        , cabal
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
    cabal =
        cabalAt (maybe 0 read (lookup "x-revision" customFieldsPD))
      where
        PackageDescription { customFieldsPD } = pkgDesc
    license = either id licenseToSPDX licenseRaw
      where
        PackageDescription { licenseRaw } = pkgDesc

fromGenericPackageDescription
    :: (Natural -> Cabal)
    -> Src
    -> GenericPackageDescription
    -> Package
fromGenericPackageDescription cabalAt src gPkgDesc =
    fromPackageDescription cabalAt src pkgDesc
  where
    GenericPackageDescription { packageDescription = pkgDesc } = gPkgDesc
