module Main (main) where

import Control.Concurrent (QSem)
import Control.Concurrent.Async (Concurrently)
import Data.Aeson ((.:))
import Data.Foldable
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.Simple.Setup (readPToMaybe)
import Distribution.Types.PackageId (PackageIdentifier (..), PackageId)
import Distribution.Types.PackageName (mkPackageName)
import Distribution.Types.PackageName (unPackageName)
import Nix.Expr ((@@))
import System.FilePath ((</>), (<.>))

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Distribution.Pretty as Pretty
import qualified Distribution.Text
import qualified Distribution.Verbosity as Verbosity
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import qualified Options.Applicative as Options
import qualified Nix.Expr
import qualified Nix.Pretty as Nix
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.IO as IO

import Hash
import Orphans ()
import Src

import qualified Cabal
import qualified Express
import qualified Options
import qualified Package.Shared as Shared

data Options =
    Options
        { allCabalHashes :: FilePath
        }

parseOptions :: Options.Parser Options
parseOptions =
    Options
        <$> Options.parseAllCabalHashes

concurrently
    :: QSem
    -> IO a
    -> Concurrently a
concurrently qsem action =
    Async.Concurrently (withQSem qsem action)

withQSem
    :: QSem
    -> IO a
    -> IO a
withQSem qsem =
    Exception.bracket_
        (Concurrent.waitQSem qsem)
        (Concurrent.signalQSem qsem)

readSrc :: FilePath -> IO Src
readSrc file =
  do
    Just value <- Aeson.decodeFileStrict file
    case Aeson.parseEither parser value of
      Left err ->
        (error . unwords) ["Failed to decode package hashes file:", file, err]
      Right src ->
        return (FromHackage src)
  where
    parser :: Aeson.Value -> Aeson.Parser Hash
    parser =
        Aeson.withObject "package hashes" parseSrc
      where
        parseSrc obj =
          do
            hashes <- obj .: "package-hashes"
            case Map.lookup sha256 hashes of
              Nothing -> fail "Missing SHA256 hash"
              Just hash -> return (Hash hash)
          where
            sha256 :: Text = "SHA256"

fromAllCabalHashes
    :: FilePath
    -> PackageId
    -> IO Shared.Package
fromAllCabalHashes allCabalHashes packageId =
  do
    sha256 <- Hash.nixHash cabal
    let cabalAt = Cabal.fromHackage sha256
    src <- readSrc json
    gPkgDescr <- readGenericPackageDescription Verbosity.silent cabal
    return (Shared.fromGenericPackageDescription cabalAt src gPkgDescr)
  where
    cabal = cabalFile allCabalHashes packageId
    json = jsonFile allCabalHashes packageId

mkdir :: FilePath -> IO ()
mkdir = Directory.createDirectoryIfMissing True

writeDoc :: FilePath -> Doc ann -> IO ()
writeDoc fileName doc =
    IO.withFile fileName IO.WriteMode (\h -> Pretty.hPutDoc h doc)

writePackage
    :: FilePath
    -> PackageId
    -> IO (Set PackageId)
writePackage allCabalHashes packageId =
    Exception.handle nonFatalErrors
      do
        package <- fromAllCabalHashes allCabalHashes packageId
        let outFile = packageFile packageId
        mkdir (FilePath.takeDirectory outFile)
        writeDoc outFile (Nix.prettyNix $ Express.express package)
        return (Set.singleton packageId)
  where
    -- Display errors, but do not abort.
    nonFatalErrors (Exception.SomeException e) =
      do
        IO.hPutStrLn IO.stderr (Exception.displayException e)
        return Set.empty

writePackages
    :: QSem
    -> FilePath
    -> FilePath
    -> Concurrently (Set PackageId)
writePackages qsem allCabalHashes package =
    concurrently qsem
      do
        packageIds <- getPackageIds allCabalHashes package
        written <- traverse (writePackage allCabalHashes) packageIds
        return (Set.unions written)

writeIndex :: Set PackageId -> IO ()
writeIndex indexed =
  do
    let outFile = "index.nix"
    writeDoc outFile (Nix.prettyNix index)
  where
    index = Nix.Expr.mkNonRecSet (mkBinding <$> toList indexed)
    mkBinding packageId =
        Nix.Expr.bindTo name importExpr
      where
        name = "\"" <> Text.pack prettyPackageId <> "\""
        prettyPackageId = Pretty.prettyShow packageId
        importExpr = Nix.Expr.mkSym "import" @@ Nix.Expr.mkRelPath packagePath
        packagePath =
            "."
                </> unPackageName pkgName
                </> prettyPackageId <.> "nix"
          where
            PackageIdentifier { pkgName } = packageId

cabalFile :: FilePath -> PackageId -> FilePath
cabalFile allCabalHashes PackageIdentifier { pkgName, pkgVersion } =
    allCabalHashes </> name </> version </> name <.> "cabal"
  where
    name = Pretty.prettyShow pkgName
    version = Pretty.prettyShow pkgVersion

jsonFile :: FilePath -> PackageId -> FilePath
jsonFile allCabalHashes PackageIdentifier { pkgName, pkgVersion } =
    allCabalHashes </> name </> version </> name <.> "json"
  where
    name = Pretty.prettyShow pkgName
    version = Pretty.prettyShow pkgVersion

packageFile :: PackageId -> FilePath
packageFile packageId@PackageIdentifier { pkgName } =
    Pretty.prettyShow pkgName </> Pretty.prettyShow packageId <.> "nix"

getPackageIds
    :: FilePath  -- ^ Cabal hashes
    -> FilePath  -- ^ Package directory
    -> IO [PackageId]
getPackageIds allCabalHashes package =
  do
    subdirs <-
        Directory.listDirectory dir
            >>= Monad.filterM isDirectory . filter (not . isHiddenFile)
    return (mapMaybe getPackageId subdirs)
  where
    dir = allCabalHashes </> package
    isDirectory this = Directory.doesDirectoryExist (dir </> this)
    getPackageId version =
        case readPToMaybe Distribution.Text.parse version of
          Nothing -> Nothing
          Just pkgVersion ->
              Just PackageIdentifier { pkgName, pkgVersion }
      where
        pkgName = mkPackageName package

isHiddenFile :: FilePath -> Bool
isHiddenFile ('.' : _) = True
isHiddenFile _ = False

getPackages :: FilePath -> IO [FilePath]
getPackages allCabalHashes =
    Directory.listDirectory allCabalHashes
        >>= Monad.filterM isDirectory . filter (not . isHiddenFile)
  where
    isDirectory dir = Directory.doesDirectoryExist (allCabalHashes </> dir)

main :: IO ()
main =
  do
    Options { allCabalHashes } <- Options.execParser parserInfo
    packages <- getPackages allCabalHashes
    njobs <- Concurrent.getNumCapabilities
    qsem <- Concurrent.newQSem njobs
    written <-
        Async.runConcurrently
            (traverse (writePackages qsem allCabalHashes) packages)
    let indexed = Set.unions written
    writeIndex indexed
    return ()
  where
    parserInfo =
        Options.info
            (parseOptions Options.<**> Options.helper)
            (mconcat
                [ Options.fullDesc
                , Options.progDesc "Derive Nix expressions for ALL_CABAL_HASHES"
                ]
            )
