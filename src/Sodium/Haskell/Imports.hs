{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Sodium.Haskell.Imports where

import Sodium.Prelude
import Data.Data
import qualified Data.Set as S
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.SrcLoc (noLoc)

imports :: Module -> Module
imports (Module srcLoc name pragmas wtext exportSpec importDecls decls) =
    let importDecls' = importDecls ++ map importDecl modules
        collected = execWriter (gcollect decls)
        modules = (delete (ModuleName "Prelude") . nub)
                [ moduleName | Qual moduleName _ <- toList collected ]
    in Module srcLoc name pragmas wtext exportSpec importDecls' decls

importDecl moduleName =
    ImportDecl noLoc moduleName
    False False False Nothing Nothing Nothing

gmapM_ :: forall a m. (Data a, Monad m) => (forall d. Data d => d -> m ()) -> a -> m ()
gmapM_ f a = gmapM (\d -> f d >> return d) a >> return ()

gcollect :: (Data a, Applicative m, MonadWriter (Set QName) m) => a -> m ()
gcollect a = case cast a of
    Nothing -> case cast a of
        Nothing -> gmapM_ gcollect a
        Just (name :: Name) -> gcollect (UnQual name)
    Just (name :: QName) -> tell (S.singleton name)
