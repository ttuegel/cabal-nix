module Main (main) where

import Control.Concurrent (QSem)
import Control.Concurrent.Async (Concurrently)
import Data.Foldable
import Data.Maybe
import Distribution.Simple.Setup (readPToMaybe)
import Distribution.Types.PackageId (PackageIdentifier (..), PackageId)
import Distribution.Types.PackageName (mkPackageName)
import System.FilePath ((</>), (<.>))

import qualified Data.Aeson as Aeson
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Distribution.Pretty as Pretty
import qualified Distribution.Text
import qualified Options.Applicative as Options
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.IO as IO

import Orphans ()

import qualified Options
import qualified Package.Shared as Package

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

writePackage
    :: FilePath
    -> PackageId
    -> IO ()
writePackage allCabalHashes packageId =
    Exception.handle nonFatalErrors
      do
        package <-
            Package.fromAllCabalHashes
                (cabalFile allCabalHashes packageId)
                (jsonFile allCabalHashes packageId)
        let outFile = packageFile packageId
        Directory.createDirectoryIfMissing
            True
            (FilePath.takeDirectory outFile)
        Aeson.encodeFile outFile package
  where
    -- Display errors, but do not abort; return an empty Map instead.
    nonFatalErrors (Exception.SomeException e) =
      do
        IO.hPutStrLn IO.stderr (Exception.displayException e)

writePackages
    :: QSem
    -> FilePath
    -> FilePath
    -> Concurrently ()
writePackages qsem allCabalHashes package =
    concurrently qsem
      do
        packageIds <- getPackageIds allCabalHashes package
        traverse_ (writePackage allCabalHashes) packageIds

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
    Pretty.prettyShow pkgName </> Pretty.prettyShow packageId <.> "json"

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
    Async.runConcurrently
        (traverse_ (writePackages qsem allCabalHashes) packages)
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
