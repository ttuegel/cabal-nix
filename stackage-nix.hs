module Main (main) where

import Data.Aeson.Types ((.:))
import Data.Foldable
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc)
import Distribution.Types.PackageId (PackageIdentifier (..))
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.PackageName (mkPackageName)
import Distribution.Version (Version)
import Nix.Expr ((@.))

import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import qualified Data.Yaml as Yaml
import qualified Distribution.Compat.ReadP as ReadP
import qualified Distribution.Pretty as Pretty
import qualified Distribution.Text
import qualified Nix.Expr
import qualified Nix.Pretty as Nix
import qualified Options.Applicative as Options
import qualified System.FilePath as FilePath
import qualified System.IO as IO

import qualified Options

data Options =
    Options
        { resolver :: FilePath
        }

parseOptions :: Options.Parser Options
parseOptions =
    Options
        <$> Options.parseResolver

readResolver :: FilePath -> IO (Map PackageName PackageIdentifier)
readResolver file =
  do
    value <- Yaml.decodeFileThrow file
    case Aeson.parseEither parser value of
      Left err ->
        (error . unwords) ["Failed to decode resolver:", file, err]
      Right result -> return result
  where
    parser :: Aeson.Value -> Aeson.Parser (Map PackageName PackageIdentifier)
    parser =
        Aeson.withObject "resolver" parseResolver
      where
        parseResolver obj =
            obj .: "packages" >>= Aeson.withObject "packages" parsePackages
        parsePackages obj =
          do
            packages <- traverse (Aeson.withObject "package" return) obj
            resolver <- traverse parsePackage (HashMap.toList packages)
            return (Map.fromList resolver)
        parsePackage
            :: (Text, Aeson.Object)
            -> Aeson.Parser (PackageName, PackageIdentifier)
        parsePackage (pkgName', pkgInfo) =
          do
            pkgVersion <- pkgInfo .: "version" >>= parseVersion
            let
              pkgName = mkPackageName (Text.unpack pkgName')
              pkgId = PackageIdentifier { pkgName, pkgVersion }
            return (pkgName, pkgId)

    parseVersion :: Text -> Aeson.Parser Version
    parseVersion =
        go . ReadP.readP_to_S Distribution.Text.parse . Text.unpack
      where
        go [(v,[])] = return v
        go (_ : xs) = go xs
        go _        = fail "could not parse Version"

writeDoc :: FilePath -> Doc ann -> IO ()
writeDoc fileName doc =
    IO.withFile fileName IO.WriteMode (\h -> Pretty.hPutDoc h doc)

writeResolver :: FilePath -> Map PackageName PackageIdentifier -> IO ()
writeResolver outFile packageIds =
    writeDoc outFile (Nix.prettyNix resolver)
  where
    resolver =
        Nix.Expr.mkFunction
            (Nix.Expr.Param "self")
            (Nix.Expr.mkNonRecSet (mkBinding <$> toList packageIds))
    mkBinding packageId =
        Nix.Expr.bindTo name (Nix.Expr.mkSym "self" @. package)
      where
        name = "\"" <> prettyPackageName <> "\""
          where
            prettyPackageName = Text.pack (Pretty.prettyShow pkgName)
            PackageIdentifier { pkgName } = packageId
        package = "\"" <> prettyPackageId <> "\""
          where
            prettyPackageId = Text.pack (Pretty.prettyShow packageId)

main :: IO ()
main =
  do
    Options { resolver } <- Options.execParser parserInfo
    packageIds <- readResolver resolver
    let
      nixExprFile =
          FilePath.replaceExtension (FilePath.takeFileName resolver) "nix"
    writeResolver nixExprFile packageIds
    return ()
  where
    parserInfo =
        Options.info
            (parseOptions Options.<**> Options.helper)
            (mconcat
                [ Options.fullDesc
                , Options.progDesc "Derive Nix expressions for RESOLVER"
                ]
            )
