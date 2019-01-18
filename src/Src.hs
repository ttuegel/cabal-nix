{-# LANGUAGE StrictData #-}

module Src where

import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Nix.Expr ((@@))
import Nix.Expr ((@.))

import qualified Nix.Expr

import Express (Express)
import Hash

import qualified Express

data Src =
    FromHackage Hash
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance Express Src where
    express src =
        Nix.Expr.mkFunction
            (Nix.Expr.Param "Src")
            case src of
              FromHackage sha256 ->
                (@@)
                    (Nix.Expr.mkSym "Src" @. "fromHackage")
                    (Express.express sha256)
