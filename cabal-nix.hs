module Main (main) where

import Data.Set (Set)
import Distribution.Compiler (CompilerInfo)
import Distribution.System (Arch (..))
import Distribution.System (OS (..))
import Distribution.System (Platform (..))

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Set as Set
import qualified Options.Applicative as Options

import Orphans ()

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
    Options { cabalFile, compilerInfo } <- Options.execParser parserInfo
    packages <- Package.forPlatforms compilerInfo platforms cabalFile
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
