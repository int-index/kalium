{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Sodium.Haskell.Imports where

import Sodium.Prelude
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.SrcLoc (noLoc)

imports :: Module -> Module
imports (Module srcLoc name pragmas wtext exportSpec importDecls decls) =
    let importDecls' = importDecls ++ map (importDecl . ModuleName)
            ["Control.Monad", "Control.Applicative", "Data.Bool"]
    in Module srcLoc name pragmas wtext exportSpec importDecls' decls

importDecl moduleName =
    ImportDecl noLoc moduleName
    False False False Nothing Nothing Nothing

class Collect a where
    collect :: MonadWriter [QName] m => a -> m ()

instance (Collect a, Traversable f) => Collect (f a) where
    collect = mapM_ collect
