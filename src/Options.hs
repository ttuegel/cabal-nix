module Options where

import Distribution.Compiler (CompilerInfo)
import Distribution.System (Platform)

import qualified Distribution.Compat.ReadP as ReadP
import qualified Distribution.Simple.Compiler as Compiler
import qualified Distribution.Text
import qualified Options.Applicative as Options

parseCabalFile :: Options.Parser FilePath
parseCabalFile =
    Options.strArgument (mconcat info)
  where
    info =
        [ Options.metavar "CABAL_FILE" ]

parseAllCabalHashes :: Options.Parser FilePath
parseAllCabalHashes =
    Options.strArgument (mconcat info)
  where
    info =
        [ Options.metavar "ALL_CABAL_HASHES" ]

parseCompilerInfo :: Options.Parser CompilerInfo
parseCompilerInfo =
    fromCompilerId <$> Options.option readCompilerId (mconcat info)
  where
    fromCompilerId compilerId =
        Compiler.unknownCompilerInfo compilerId abiTag
      where
        abiTag = Compiler.NoAbiTag
    info =
        [ Options.long "compiler"
        , Options.metavar "COMPILER"
        , Options.help
            "Generate package configuration for COMPILER (FLAVOR-[VERSION])"
        ]
    readCompilerId = readText

readText :: Distribution.Text.Text a => Options.ReadM a
readText =
    Options.maybeReader
        (\str ->
          case ReadP.readP_to_S Distribution.Text.parse str of
            [] -> Nothing
            (parsed, _) : _ -> Just parsed
        )

parsePlatform :: Options.Parser Platform
parsePlatform =
    Options.option readPlatform (mconcat info)
  where
    info =
        [ Options.long "platform"
        , Options.metavar "PLATFORM"
        , Options.help "Generate package configuration for PLATFORM (ARCH-OS)"
        ]
    readPlatform = readText
