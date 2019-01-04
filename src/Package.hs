module Package
    ( Package (..)
    , forPlatform
    , forPlatforms
    ) where

import Data.Aeson (ToJSON)
import Data.Aeson.Types ((.=))
import Data.Data (Data)
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Distribution.Compiler (CompilerInfo)
import Distribution.License (licenseToSPDX)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.SPDX.License (License)
import Distribution.System (Platform (..))
import Distribution.Types.Benchmark (Benchmark (..))
import Distribution.Types.BuildInfo (BuildInfo (..))
import Distribution.Types.ComponentRequestedSpec (ComponentRequestedSpec (..))
import Distribution.Types.Executable (Executable (..))
import Distribution.Types.GenericPackageDescription (FlagAssignment, unFlagAssignment)
import Distribution.Types.GenericPackageDescription (FlagName)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Types.Library (Library (..))
import Distribution.Types.PackageDescription (PackageDescription (..))
import Distribution.Types.PackageId (PackageIdentifier (..))
import Distribution.Types.SetupBuildInfo (SetupBuildInfo (..))
import Distribution.Types.TestSuite (TestSuite (..))
import Distribution.Types.UnqualComponentName (unUnqualComponentName)

import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Distribution.PackageDescription.Configuration as PackageDescription
import qualified Distribution.Pretty as Pretty
import qualified Distribution.Simple.BuildToolDepends as BuildToolDepends
import qualified Distribution.Verbosity as Verbosity
import qualified System.FilePath as FilePath
import qualified System.IO as IO

import Depends (Depends)
import Hash
import Orphans ()
import Revision
import Src

import qualified Depends

data Package =
    Package
        { package :: PackageIdentifier
        , revision :: Maybe Revision
        , src :: Src
        , libraries :: Map String Depends
        , executables :: Map String Depends
        , tests :: Map String Depends
        , benchmarks :: Map String Depends
        , setup :: Depends
        , flags :: Map FlagName Bool
        , license :: License
        , homepage :: String
        , synopsis :: String
        }
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance ToJSON Package where
    toJSON pkg =
        Aeson.object
            [ "pname" .= Aeson.toJSON pkgName
            , "version" .= Aeson.toJSON pkgVersion
            , "revision" .= Aeson.toJSON revision
            , "src" .= Aeson.toJSON src
            , "libraries" .= Aeson.toJSON libraries
            , "executables" .= Aeson.toJSON executables
            , "tests" .= Aeson.toJSON tests
            , "benchmarks" .= Aeson.toJSON benchmarks
            , "setup" .= Aeson.toJSON setup
            , "flags" .= Aeson.toJSON flags
            , "license" .= Pretty.prettyShow license
            , "homepage" .= Aeson.toJSON homepage
            , "synopsis" .= Aeson.toJSON synopsis
            ]
      where
        PackageIdentifier { pkgName, pkgVersion } = package
          where
            Package { package } = pkg
        Package { revision, src } = pkg
        Package { libraries, executables, tests, benchmarks, setup } = pkg
        Package { flags } = pkg
        Package { license, homepage, synopsis } = pkg

fromPackageDescription
    :: Hash  -- ^ Hash of the Cabal package description
    -> Src
    -> (PackageDescription, FlagAssignment)
    -> Package
fromPackageDescription cabalHash src (pkgDesc, flagAssignment) =
    Package
        { package
        , revision
        , src
        , libraries
        , executables
        , tests
        , benchmarks
        , setup
        , flags
        , license
        , homepage
        , synopsis
        }
  where
    PackageDescription { package, homepage, synopsis } = pkgDesc
    packageName = Pretty.prettyShow pkgName
      where
        PackageIdentifier { pkgName } = package
    fromBuildInfo buildInfo =
        mconcat
            [ mconcat (Depends.fromExeDependency <$> externalToolDepends)
            , mconcat (Depends.fromPkgconfigDependency <$> pkgconfigDepends)
            , mconcat (Depends.fromDependency <$> targetBuildDepends)
            ]
      where
        externalToolDepends =
            filter
                (not . BuildToolDepends.isInternal pkgDesc)
                (BuildToolDepends.getAllToolDependencies pkgDesc buildInfo)
        BuildInfo { pkgconfigDepends } = buildInfo
        BuildInfo { targetBuildDepends } = buildInfo
    revision =
      case lookup "x-revision" customFieldsPD of
        Nothing ->
            Nothing
        Just n ->
            Just Revision { revision = read n, hash = cabalHash }
      where
        PackageDescription { customFieldsPD } = pkgDesc
    libraries =
        Map.fromList (fromLibrary <$> maybe id (:) library subLibraries)
      where
        PackageDescription { library, subLibraries } = pkgDesc
        fromLibrary Library { libName, libBuildInfo } =
            ( maybe packageName unUnqualComponentName libName
            , fromBuildInfo libBuildInfo
            )
    executables =
        Map.fromList (fromExecutable <$> executables')
      where
        PackageDescription { executables = executables' } = pkgDesc
        fromExecutable Executable { exeName, buildInfo } =
            ( unUnqualComponentName exeName
            , fromBuildInfo buildInfo
            )
    tests =
        Map.fromList (fromTestSuite <$> testSuites)
      where
        PackageDescription { testSuites } = pkgDesc
        fromTestSuite TestSuite { testName, testBuildInfo } =
            ( unUnqualComponentName testName
            , fromBuildInfo testBuildInfo
            )
    benchmarks =
        Map.fromList (fromBenchmark <$> benchmarks')
      where
        PackageDescription { benchmarks = benchmarks' } = pkgDesc
        fromBenchmark Benchmark { benchmarkName, benchmarkBuildInfo } =
            ( unUnqualComponentName benchmarkName
            , fromBuildInfo benchmarkBuildInfo
            )
    setup =
        fromMaybe mempty (fromSetupBuildInfo <$> setupBuildInfo)
      where
        PackageDescription { setupBuildInfo } = pkgDesc
        fromSetupBuildInfo SetupBuildInfo { setupDepends } =
            mconcat (Depends.fromDependency <$> setupDepends)
    flags = Map.fromList (unFlagAssignment flagAssignment)
    license = either id licenseToSPDX licenseRaw
      where
        PackageDescription { licenseRaw } = pkgDesc

finalize
    :: CompilerInfo
    -> GenericPackageDescription
    -> Platform
    -> (PackageDescription, FlagAssignment)
finalize compilerInfo gPkgDesc platform =
  case finalized of
    Left missing ->
        error ("Missing dependencies: " ++ show missing)
    Right result ->
        result
  where
    finalized =
      PackageDescription.finalizePD
          (mempty :: FlagAssignment)
          componentRequestedSpec
          dependencySatisfiable
          platform
          compilerInfo
          extraConstraints
          gPkgDesc
    componentRequestedSpec =
        ComponentRequestedSpec
            { testsRequested = True
            , benchmarksRequested = True
            }
    dependencySatisfiable _ = True
    extraConstraints = []

forPlatform
    :: Hash
    -> Src
    -> CompilerInfo
    -> GenericPackageDescription
    -> Platform
    -> Package
forPlatform cabalHash src compilerInfo gPkgDesc platform =
    fromPackageDescription cabalHash src finalized
  where
    finalized = finalize compilerInfo gPkgDesc platform

forPlatforms
    :: CompilerInfo
    -> Set Platform  -- ^ Supported platforms
    -> FilePath  -- ^ Cabal package description file
    -> IO (Map Platform Package)
forPlatforms compilerInfo platforms cabalFile =
    Exception.handle nonFatalErrors
      (do
        cabalHash <- nixHash cabalFile
        let hashesFile = FilePath.replaceExtension cabalFile "json"
        src <- getSrc hashesFile
        gPkgDesc <- readGenericPackageDescription Verbosity.silent cabalFile
        let
          forPlatform' = Package.forPlatform cabalHash src compilerInfo gPkgDesc
          packages = Map.fromSet forPlatform' platforms
        return packages
      )
  where
    -- Display errors, but do not abort; return an empty Map instead.
    nonFatalErrors (Exception.SomeException e) =
      do
        IO.hPutStrLn IO.stderr (Exception.displayException e)
        return Map.empty
