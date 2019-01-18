{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Express where

import Data.Set (Set)
import Data.Text (Text)
import Nix.Expr (NExpr)
import Nix.Expr (($=))

import qualified Data.Map.Strict as Map.Strict
import qualified Nix.Expr

class Express a where
    express :: a -> NExpr

instance Express NExpr where
    express = id

instance Express () where
    express = const Nix.Expr.mkNull

instance Express Bool where
    express =
      \case
        True -> Nix.Expr.mkSym "true"
        False -> Nix.Expr.mkSym "false"

instance (Express a, k ~ Text) => Express (Map.Strict.Map k a) where
    express =
        Nix.Expr.mkNonRecSet . map mkBinding . Map.Strict.toList
      where
        quoted text = "\"" <> text <> "\""
        mkBinding (key, value) =
            quoted key $= express value

instance k ~ Text => Express (Set k) where
    express = express . Map.Strict.fromSet (const ())


