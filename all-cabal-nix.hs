module Main (main) where

import Control.Concurrent (QSem)
import Control.Concurrent.Async (Concurrently)
import Data.Foldable
import Data.Map.Strict (Map)
import Data.Set (Set)
import Distribution.Compiler (CompilerInfo)
import Distribution.Simple.Setup (readPToMaybe)
import Distribution.System (Arch (..))
import Distribution.System (OS (..))
import Distribution.System (Platform (..))
import Distribution.Types.PackageId (PackageIdentifier (..), PackageId)
import Distribution.Types.PackageName (mkPackageName)
import Pipes ((>->))
import System.FilePath ((</>), (<.>))

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Control.Monad.State.Strict as State
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Distribution.Pretty as Pretty
import qualified Distribution.Text
import qualified Options.Applicative as Options
import qualified Pipes as Pipes
import qualified Pipes.Aeson.Unchecked as Pipes.Aeson
import qualified Pipes.ByteString as Pipes.ByteString
import qualified System.Directory as Directory
import qualified System.IO as IO

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

forPlatforms
    :: FilePath -- ^ Cabal hashes
    -> CompilerInfo  -- ^ Selected compiler
    -> PackageId
    -> IO (Map Platform Package)
forPlatforms allCabalHashes compilerInfo packageId =
     (Package.forPlatforms compilerInfo platforms cabalFile)
  where
    cabalFile =
        allCabalHashes </> package </> version </> package <.> "cabal"
      where
        PackageIdentifier { pkgName, pkgVersion } = packageId
        package = Pretty.prettyShow pkgName
        version = Pretty.prettyShow pkgVersion

writePackageJSON
    :: QSem
    -> FilePath
    -> CompilerInfo
    -> PackageId
    -> Concurrently ()
writePackageJSON qsem allCabalHashes compilerInfo packageId =
    concurrently qsem
        (IO.withFile (packageJSON allCabalHashes packageId) IO.WriteMode
              (\hndl ->
                do
                  package <- forPlatforms allCabalHashes compilerInfo packageId
                  let outp = Pipes.ByteString.toHandle hndl
                  Pipes.runEffect (Pipes.Aeson.encode package >-> outp)
              )
        )

packageJSON :: FilePath -> PackageId -> FilePath
packageJSON allCabalHashes packageId@PackageIdentifier { pkgName } =
    allCabalHashes
        </> Pretty.prettyShow pkgName
        </> Pretty.prettyShow packageId <.> "json"

readPackageJSON
    :: QSem
    -> FilePath
    -> PackageId
    -> Concurrently Aeson.Object
readPackageJSON qsem allCabalHashes packageId =
    concurrently qsem
        (IO.withFile (packageJSON allCabalHashes packageId) IO.ReadMode
            (\hndl ->
              do
                let inp = Pipes.ByteString.fromHandle hndl
                r <- State.evalStateT Pipes.Aeson.decode inp
                case r of
                  Nothing ->
                      error "Not enough input"
                  Just (Left decodeError) ->
                      error (Exception.displayException decodeError)
                  Just (Right object) ->
                      return object
            )
        )

getPackageIds
    :: FilePath  -- ^ Cabal hashes
    -> FilePath  -- ^ Package directory
    -> IO (Set PackageId)
getPackageIds allCabalHashes package =
  do
    subdirs <-
        Directory.listDirectory dir
            >>= Monad.filterM isDirectory . filter (not . isHiddenFile)
    (return . Set.unions) (getPackageId <$> subdirs)
  where
    dir = allCabalHashes </> package
    isDirectory this = Directory.doesDirectoryExist (dir </> this)
    getPackageId version =
        case readPToMaybe Distribution.Text.parse version of
          Nothing -> Set.empty
          Just pkgVersion ->
              Set.singleton PackageIdentifier { pkgName, pkgVersion }
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
    njobs <- Concurrent.getNumCapabilities
    qsem <- Concurrent.newQSem njobs
    allPackageIds <-
        mconcat <$> traverse (getPackageIds allCabalHashes) packages
    Async.runConcurrently
        (traverse_
            (writePackageJSON qsem allCabalHashes compilerInfo)
            allPackageIds
        )
    allPackages <-
        Async.runConcurrently
            (traverse
                (readPackageJSON qsem allCabalHashes)
                (Map.fromSet id allPackageIds)
            )
    Pipes.runEffect
        (Pipes.Aeson.encode allPackages >-> Pipes.ByteString.stdout)
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
