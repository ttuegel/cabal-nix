{-# LANGUAGE StrictData #-}

module Cabal
    ( Cabal (..)
    , fromHackage
    ) where

import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Nix.Expr ((@@))
import Nix.Expr ((@.))

import qualified Nix.Expr

import Express (Express)
import Hash
import Revision

import qualified Express

data Cabal =
    FromHackage Revision
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance Express Cabal where
    express cabal =
        Nix.Expr.mkFunction
            (Nix.Expr.Param "Cabal")
            case cabal of
              FromHackage rev ->
                (@@)
                    (Nix.Expr.mkSym "Cabal" @. "fromHackage")
                    (Express.express rev)

fromHackage :: Hash -> Natural -> Cabal
fromHackage sha256 revision = FromHackage Revision { revision, sha256 }
