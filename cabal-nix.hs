module Main (main) where

import Data.Set (Set)
import Distribution.Compiler (CompilerId)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.System (Arch (..))
import Distribution.System (OS (..))
import Distribution.System (Platform (..))

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Distribution.Compat.ReadP as ReadP
import qualified Distribution.Simple.Compiler as Compiler
import qualified Distribution.Text
import qualified Distribution.Verbosity as Verbosity
import qualified Options.Applicative as Options
import qualified System.FilePath as FilePath

import Hash
import Orphans ()

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
        , compilerId :: CompilerId
        }

parseOptions :: Options.Parser Options
parseOptions =
    Options
        <$> parseCabalFile
        <*> parseCompilerId
  where
    parseCabalFile =
        Options.strArgument (mconcat info)
      where
        info =
            [ Options.metavar "CABAL_FILE" ]
    parseCompilerId =
        Options.option readCompilerId (mconcat info)
      where
        info =
            [ Options.long "compiler"
            , Options.metavar "COMPILER"
            , Options.help
                "Generate package configuration for COMPILER (FLAVOR-[VERSION])"
            ]
        readCompilerId =
            Options.maybeReader
                (\str ->
                  case ReadP.readP_to_S Distribution.Text.parse str of
                    [] -> Nothing
                    (compilerId, _) : _ -> Just compilerId
                )

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
          compilerInfo = Compiler.unknownCompilerInfo compilerId abiTag
            where
              abiTag = Compiler.NoAbiTag
              Options { compilerId } = options
          forPlatform' =
              Package.forPlatform cabalHash src compilerInfo gPkgDesc
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
