module Hash where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.String
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import qualified Data.Aeson as Aeson
import qualified System.Process as Process

newtype Hash = Hash { getHash :: Text }
  deriving (Data, Eq, Generic, IsString, Ord, Read, Show, Typeable)

instance FromJSON Hash where
    parseJSON = Aeson.withText "hash" (return . Hash)

instance ToJSON Hash where
    toJSON = Aeson.toJSON . getHash

nixHash :: FilePath -> IO Hash
nixHash file =
  do
    let
      args = [ "--type", "sha256", "--base32", file ]
      inp = mempty
    unpacked <- Process.readProcess "nix-hash" args inp
    case lines unpacked of
      [] -> error "nix-hash did not return a hash"
      hash : _ -> return (fromString hash)
