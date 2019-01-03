module Main (main) where

import Data.Map.Strict (Map)
import Data.Maybe
import Data.Set (Set)
import Distribution.Compiler (CompilerInfo)
import Distribution.Simple.Setup (readPToMaybe)
import Distribution.System (Arch (..))
import Distribution.System (OS (..))
import Distribution.System (Platform (..))
import Distribution.Types.PackageId (PackageIdentifier (..), PackageId)
import Distribution.Types.PackageName (mkPackageName)
import System.FilePath ((</>), (<.>))

import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Distribution.Pretty as Pretty
import qualified Distribution.Text
import qualified Options.Applicative as Options
import qualified System.Directory as Directory

import Orphans ()
import Package (Package)

import qualified Options
import qualified Package

platforms :: Set Platform
platforms =
    Set.fromList
        [ Platform I386 Linux
        , Platform X86_64 Linux
        , Platform X86_64 OSX
        ]

data Options =
    Options
        { allCabalHashes :: FilePath
        , compilerInfo :: CompilerInfo
        }

parseOptions :: Options.Parser Options
parseOptions =
    Options
        <$> Options.parseAllCabalHashes
        <*> Options.parseCompilerInfo

forPlatforms
    :: FilePath  -- ^ Cabal hashes
    -> CompilerInfo
    -> PackageId
    -> IO (Map Platform Package)
forPlatforms allCabalHashes compilerInfo packageId =
    Package.forPlatforms compilerInfo platforms cabalFile
  where
    cabalFile =
        allCabalHashes </> package </> version </> package <.> "cabal"
      where
        PackageIdentifier { pkgName, pkgVersion } = packageId
        package = Pretty.prettyShow pkgName
        version = Pretty.prettyShow pkgVersion

getPackageIds
    :: FilePath  -- ^ Cabal hashes
    -> FilePath  -- ^ Package directory
    -> IO (Set PackageId)
getPackageIds allCabalHashes package =
    Set.fromList . mapMaybe getPackageId <$> Directory.listDirectory dir
  where
    dir = allCabalHashes </> package
    getPackageId version =
        case readPToMaybe Distribution.Text.parse version of
          Nothing -> Nothing
          Just pkgVersion -> Just PackageIdentifier { pkgName, pkgVersion }
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
    Options { allCabalHashes, compilerInfo } <- Options.execParser parserInfo
    packages <- getPackages allCabalHashes
    allPackageIds <-
        mconcat <$> traverse (getPackageIds allCabalHashes) packages
    allPackages <-
        traverse
            (forPlatforms allCabalHashes compilerInfo)
            (Map.fromSet id allPackageIds)
    ByteString.putStr (Aeson.encode allPackages)
    putStr "\n"
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
