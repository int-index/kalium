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
        IfStatement      a -> BodyStatement <$> side' a
        BodyStatement    a -> BodyStatement <$> side' a


class Side' a where
    side' :: NameStack t m => a Expression -> m (Body Atom)

instance Side' Body where
    side' = side

instance Side' If where
    side' (If cond thenb elseb) = sideStatement $
        If <$> sideExpression cond
           <*> side thenb
           <*> side elseb


instance Side' ForCycle where
    side' (ForCycle name range body) = sideStatement $
        ForCycle name <$> sideExpression range <*> side body

instance Side' Exec where
    side' (Exec mname op exprs) = sideStatement $
        Exec mname op <$> mapM sideExpression exprs

sideStatement w = do
    (a, unzip -> (vardecls, sidecalls)) <- runWriterT w
    return $ Body (M.fromList vardecls) (sidecalls `snoc` statement a)

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
