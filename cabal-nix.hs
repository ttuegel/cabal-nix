module Main (main) where

import Data.Set (Set)
import Distribution.Compiler (CompilerInfo)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.System (Arch (..))
import Distribution.System (OS (..))
import Distribution.System (Platform (..))

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Distribution.Verbosity as Verbosity
import qualified Options.Applicative as Options
import qualified System.FilePath as FilePath

import Hash
import Orphans ()

import qualified Options
import qualified Package
import qualified Src

platforms :: Set Platform
platforms =
    Set.fromList
        [ Platform I386 Linux
        , Platform X86_64 Linux
        , Platform X86_64 OSX
        ]

data Options =
    Options
        { cabalFile :: FilePath
        , compilerInfo :: CompilerInfo
        }

parseOptions :: Options.Parser Options
parseOptions =
    Options
        <$> Options.parseCabalFile
        <*> Options.parseCompilerInfo

main :: IO ()
main =
  do
    options@Options { cabalFile } <- Options.execParser parserInfo
    cabalHash <- nixHash cabalFile
    let hashesFile = FilePath.replaceExtension cabalFile "json"
    src <- Src.getSrc hashesFile
    gPkgDesc <- readGenericPackageDescription Verbosity.normal cabalFile
    let
      packages = Map.fromSet forPlatform' platforms
        where
          Options { compilerInfo } = options
          forPlatform' = Package.forPlatform cabalHash src compilerInfo gPkgDesc
    ByteString.putStr (Aeson.encode packages)
    putStr "\n"
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
