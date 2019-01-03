module Main (main) where

import Data.Aeson (FromJSON)
import Data.Aeson.Types ((.:))
import Data.Data (Data)
import Data.Foldable
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Set (Set)
import Data.String
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Distribution.Compiler (CompilerFlavor)
import Distribution.Compiler (CompilerInfo)
import Distribution.License (licenseToSPDX)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.Simple.Program (defaultProgramDb)
import Distribution.SPDX.License (License)
import Distribution.System (Arch (..))
import Distribution.System (OS (..))
import Distribution.System (Platform (..))
import Distribution.Types.Benchmark (Benchmark (..))
import Distribution.Types.BuildInfo (BuildInfo (..))
import Distribution.Types.ComponentRequestedSpec (ComponentRequestedSpec (..))
import Distribution.Types.Dependency (Dependency (..))
import Distribution.Types.Executable (Executable (..))
import Distribution.Types.ExeDependency (ExeDependency (..))
import Distribution.Types.GenericPackageDescription (FlagAssignment, unFlagAssignment)
import Distribution.Types.GenericPackageDescription (FlagName)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Types.Library (Library (..))
import Distribution.Types.PackageDescription (PackageDescription (..))
import Distribution.Types.PackageId (PackageIdentifier (..))
import Distribution.Types.PackageName (PackageName, unPackageName)
import Distribution.Types.PkgconfigDependency (PkgconfigDependency (..))
import Distribution.Types.PkgconfigName (PkgconfigName)
import Distribution.Types.SetupBuildInfo (SetupBuildInfo (..))
import Distribution.Types.TestSuite (TestSuite (..))
import Distribution.Types.UnqualComponentName (unUnqualComponentName)

import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Distribution.PackageDescription.Configuration as PackageDescription
import qualified Distribution.Simple.BuildToolDepends as BuildToolDepends
import qualified Distribution.Simple.Compiler as Compiler
import qualified Distribution.Simple.Configure as Configure
import qualified Distribution.Verbosity as Verbosity
import qualified Options.Applicative as Options
import qualified System.FilePath as FilePath
import qualified System.Process as Process

newtype Hash = Hash { getHash :: Text }
  deriving (Data, Eq, Generic, IsString, Ord, Read, Show, Typeable)

instance FromJSON Hash where
    parseJSON = Aeson.withText "hash" (return . Hash)

data Revision =
    Revision
        { revision :: Natural
        , hash :: Hash  -- ^ Cabal package description hash
        }
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

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

data Depends =
    Depends
        { toolDepends :: Set PackageName
        , pkgconfigDepends :: Set PkgconfigName
        , buildDepends :: Set PackageName
        }
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

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
        , flags :: [(FlagName, Bool)]
        , license :: License
        , homepage :: String
        , synopsis :: String
        }
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

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
    packageName = unPackageName pkgName
      where
        PackageIdentifier { pkgName } = package
    fromBuildInfo buildInfo =
        mconcat
            [ mconcat (fromExeDependency <$> externalToolDepends)
            , mconcat (fromPkgconfigDependency <$> pkgconfigDepends)
            , mconcat (fromDependency <$> targetBuildDepends)
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
            mconcat (fromDependency <$> setupDepends)
    flags = unFlagAssignment flagAssignment
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

getCompilerInfo :: CompilerFlavor -> IO CompilerInfo
getCompilerInfo flavor =
  do
    (compiler, _, _) <-
        Configure.configCompilerEx
            (Just flavor)
            Nothing
            Nothing
            defaultProgramDb
            Verbosity.normal
    return (Compiler.compilerInfo compiler)

platforms :: Set Platform
platforms =
    Set.fromList
        [ Platform I386 Linux
        , Platform X86_64 Linux
        , Platform X86_64 OSX
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

nixHash :: FilePath -> IO Hash
nixHash file =
  do
    let
      args = [ "--type", "sha256", "--base32", file ]
      inp = mempty
    unpacked <- Process.readProcess "nix-hash" args inp
    case lines unpacked of
      [] -> error "nix-hash did not return a hash"
      hash : _ -> return (fromString hash)

packageForPlatform
    :: Hash
    -> Src
    -> CompilerInfo
    -> GenericPackageDescription
    -> Platform
    -> Package
packageForPlatform cabalHash src compilerInfo gPkgDesc platform =
    fromPackageDescription
        cabalHash
        src
        (finalize compilerInfo gPkgDesc platform)

data Options =
    Options
        { cabalFile :: FilePath
        }

parseOptions :: Options.Parser Options
parseOptions =
    Options
        <$> parseCabalFile
  where
    parseCabalFile =
        Options.strArgument (mconcat info)
      where
        info =
            [ Options.metavar "CABAL_FILE" ]

main :: IO ()
main =
  do
    Options { cabalFile } <- Options.execParser parserInfo
    cabalHash <- nixHash cabalFile
    let hashesFile = FilePath.replaceExtension cabalFile "json"
    src <- getSrc hashesFile
    compilerInfo <- getCompilerInfo Compiler.GHC
    gPkgDesc <- readGenericPackageDescription Verbosity.normal cabalFile
    let
      packages = Map.fromSet packageForPlatform' platforms
        where
          packageForPlatform' =
              packageForPlatform cabalHash src compilerInfo gPkgDesc
    print packages
    return ()
  where
    parserInfo =
        Options.info
            (parseOptions Options.<**> Options.helper)
            (mconcat
                [ Options.fullDesc
                , Options.progDesc "Convert CABAL_FILE to a Nix expression"
                ]
            )
