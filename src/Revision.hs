{-# LANGUAGE StrictData #-}

module Revision where

import Data.Aeson (ToJSON)
import Data.Aeson ((.=))
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Nix.Expr (($=))
import Numeric.Natural (Natural)

import qualified Data.Aeson as Aeson
import qualified Nix.Expr

import Express (Express)
import Hash

import qualified Express

data Revision =
    Revision
        { revision :: Natural
        , hash :: Hash  -- ^ Cabal package description hash
        }
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance Express Revision where
    express rev =
        Nix.Expr.mkNonRecSet
            [ "revision" $= Express.express revision
            , "sha256" $= Express.express hash
            ]
      where
        Revision { revision, hash } = rev

instance ToJSON Revision where
    toJSON Revision { revision, hash } =
        Aeson.object
            [ "revision" .= revision
            , "hash" .= hash
            ]

    toEncoding Revision { revision, hash } =
        (Aeson.pairs . mconcat)
            [ "revision" .= revision
            , "hash" .= hash
            ]
