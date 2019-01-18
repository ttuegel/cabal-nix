{-# LANGUAGE StrictData #-}

module Depends where

import Data.Aeson (ToJSON)
import Data.Aeson.Types ((.=))
import Data.Data (Data)
import Data.Set (Set)
import Data.Typeable (Typeable)
import Distribution.Types.Dependency (Dependency (..))
import Distribution.Types.ExeDependency (ExeDependency (..))
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.PackageName (unPackageName)
import Distribution.Types.PkgconfigDependency (PkgconfigDependency (..))
import Distribution.Types.PkgconfigName (PkgconfigName)
import Distribution.Types.PkgconfigName (unPkgconfigName)
import GHC.Generics (Generic)
import Nix.Expr (($=))

import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Distribution.Pretty as Pretty
import qualified Nix.Expr

import Express (Express)

import qualified Express

data Depends =
    Depends
        { toolDepends :: Set PackageName
        , buildDepends :: Set PackageName
        , pkgconfigDepends :: Set PkgconfigName
        }
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance Express Depends where
    express depends =
        Nix.Expr.mkNonRecSet
            [ "buildDepends" $= expressPackages buildDepends
            , "toolDepends" $= expressPackages toolDepends
            , "pkgconfigDepends" $= expressPkgconfigs pkgconfigDepends
            ]
      where
        Depends { toolDepends, pkgconfigDepends, buildDepends } = depends
        expressPackages = Express.express . Set.map (Text.pack . unPackageName)
        expressPkgconfigs =
            Express.express . Set.map (Text.pack . unPkgconfigName)

instance ToJSON Depends where
    toJSON depends =
        Aeson.object
            [ "toolDepends" .= toolDependsValue
            , "pkgconfigDepends" .= pkgconfigDependsValue
            , "buildDepends" .= buildDependsValue
            ]
      where
        toolDependsValue =
            Map.fromSet id (Set.map Pretty.prettyShow toolDepends)
          where
            Depends { toolDepends } = depends
        pkgconfigDependsValue =
            Map.fromSet id (Set.map Pretty.prettyShow pkgconfigDepends)
          where
            Depends { pkgconfigDepends } = depends
        buildDependsValue =
            Map.fromSet id (Set.map Pretty.prettyShow buildDepends)
          where
            Depends { buildDepends } = depends

    toEncoding depends =
        (Aeson.pairs . mconcat)
            [ "toolDepends" .= toolDependsValue
            , "pkgconfigDepends" .= pkgconfigDependsValue
            , "buildDepends" .= buildDependsValue
            ]
      where
        toolDependsValue =
            Map.fromSet id (Set.map Pretty.prettyShow toolDepends)
          where
            Depends { toolDepends } = depends
        pkgconfigDependsValue =
            Map.fromSet id (Set.map Pretty.prettyShow pkgconfigDepends)
          where
            Depends { pkgconfigDepends } = depends
        buildDependsValue =
            Map.fromSet id (Set.map Pretty.prettyShow buildDepends)
          where
            Depends { buildDepends } = depends

instance Semigroup Depends where
    (<>) a b =
        Depends
            { toolDepends =
              let
                Depends { toolDepends = depends1 } = a
                Depends { toolDepends = depends2 } = b
              in
                depends1 <> depends2
            , pkgconfigDepends =
              let
                Depends { pkgconfigDepends = depends1 } = a
                Depends { pkgconfigDepends = depends2 } = b
              in
                depends1 <> depends2
            , buildDepends =
              let
                Depends { buildDepends = depends1 } = a
                Depends { buildDepends = depends2 } = b
              in
                depends1 <> depends2
            }

instance Monoid Depends where
    mempty =
        Depends
            { toolDepends = mempty
            , pkgconfigDepends = mempty
            , buildDepends = mempty
            }
    mappend = (<>)

fromExeDependency :: ExeDependency -> Depends
fromExeDependency (ExeDependency depend _ _) =
    mempty { toolDepends = Set.singleton depend }

fromPkgconfigDependency :: PkgconfigDependency -> Depends
fromPkgconfigDependency (PkgconfigDependency depend _) =
    mempty { pkgconfigDepends = Set.singleton depend }

fromDependency :: Dependency -> Depends
fromDependency (Dependency depend _) =
    mempty { buildDepends = Set.singleton depend }

null :: Depends -> Bool
null Depends { buildDepends, toolDepends, pkgconfigDepends } =
    Set.null buildDepends && Set.null toolDepends && Set.null pkgconfigDepends
