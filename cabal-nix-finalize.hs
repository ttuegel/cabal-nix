module Main (main) where

import Distribution.Compiler (CompilerInfo)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.System (Platform (..))

import qualified Distribution.Verbosity as Verbosity
import qualified Options.Applicative as Options
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import qualified Nix.Pretty as Nix

import Orphans ()

import qualified Express
import qualified Options
import qualified Package

data Options =
    Options
        { cabalFile :: FilePath
        , compilerInfo :: CompilerInfo
        , platform :: Platform
        }

parseOptions :: Options.Parser Options
parseOptions =
    Options
        <$> Options.parseCabalFile
        <*> Options.parseCompilerInfo
        <*> Options.parsePlatform

main :: IO ()
main =
  do
    options <- Options.execParser parserInfo
    let Options { cabalFile, compilerInfo, platform } = options
    gPkgDesc <- readGenericPackageDescription Verbosity.silent cabalFile
    let package = Package.forPlatform compilerInfo gPkgDesc platform
    (Pretty.putDoc . Nix.prettyNix) (Express.express package)
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
