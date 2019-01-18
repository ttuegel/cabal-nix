{-# LANGUAGE StrictData #-}

module Revision
    ( Revision (..)
    , module Numeric.Natural
    ) where

import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Nix.Expr (($=))
import Numeric.Natural (Natural)

import qualified Nix.Expr

import Express (Express)
import Hash

import qualified Express

data Revision =
    Revision
        { revision :: Natural
        , sha256 :: Hash  -- ^ Cabal package description hash
        }
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance Express Revision where
    express rev =
        Nix.Expr.mkNonRecSet
            [ "revision" $= Express.express revision
            , "sha256" $= Express.express sha256
            ]
      where
        Revision { revision, sha256 } = rev
