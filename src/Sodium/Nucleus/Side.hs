{-# LANGUAGE FlexibleContexts, ConstraintKinds, ViewPatterns #-}
module Sodium.Nucleus.Side (side) where

import Control.Lens
import Control.Monad.Writer
import Control.Applicative
import qualified Data.Map as M
import Sodium.Nucleus.Program.Scalar
import Sodium.Nucleus.Name

type VarDecl = (Name, Type)

class Side a where
    side :: NameStack t m => a Expression -> m (a Atom)

instance Side Program where side = (programFuncs . traversed) side
instance Side Func    where side = funcBody side
instance Side Body    where side = (bodyStatements . traversed) side

instance Side Statement where
    side = \case
        Execute          a -> BodyStatement <$> side' a
        ForStatement     a -> BodyStatement <$> side' a
        MultiIfStatement a -> BodyStatement <$> side' a
        BodyStatement    a -> BodyStatement <$> side' a


class Side' a where
    side' :: NameStack t m => a Expression -> m (Body Atom)

instance Side' Body where
    side' = side

instance Side' MultiIf where
    side' (MultiIf leafs) =
        MultiIf `sideWith` (traverse (_1 sideExpression >=> _2 side) leafs)

instance Side' ForCycle where
    side' (ForCycle name range body) =
        uncurry (ForCycle name) `sideWith`
            liftA2 (,) (sideExpression range) (side body)

instance Side' Exec where
    side' (Exec mname op exprs) =
        Exec mname op `sideWith` mapM sideExpression exprs

sideWith k w = do
    (a, unzip -> (vardecls, sidecalls)) <- runWriterT w
    return $ Body (M.fromList vardecls) (sidecalls `snoc` statement (k a))

sideExpression :: NameStack t m => Expression
               -> WriterT [(VarDecl, Statement Atom)] m Atom
sideExpression = \case
    Atom atom -> return atom
    Call op args -> do
        eArgs <- mapM sideExpression args
        name  <- namepop
        let vardecl = (name, TypeUnit) -- TODO: the real type
        tell [(vardecl, Execute $ Exec (Just name) op eArgs)]
        return (Access name)
