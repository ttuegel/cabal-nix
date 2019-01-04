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
import Distribution.Types.PkgconfigDependency (PkgconfigDependency (..))
import Distribution.Types.PkgconfigName (PkgconfigName)
import GHC.Generics (Generic)

import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Distribution.Pretty as Pretty

data Depends =
    Depends
        { toolDepends :: Set PackageName
        , pkgconfigDepends :: Set PkgconfigName
        , buildDepends :: Set PackageName
        }
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

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
