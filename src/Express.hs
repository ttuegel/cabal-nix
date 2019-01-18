{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Express where

import Data.Set (Set)
import Data.Text (Text)
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.Version (Version)
import Nix.Expr (NExpr)
import Nix.Expr (($=))

import qualified Data.Map.Strict as Map.Strict
import qualified Data.Text as Text
import qualified Distribution.Text
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

instance Express Text where
    express = Nix.Expr.mkStr

instance Express PackageName where
    express = express . Text.pack . Distribution.Text.display

instance Express Version where
    express = express . Text.pack . Distribution.Text.display
