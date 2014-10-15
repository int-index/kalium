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
instance Side Func    where side = funcScope side
instance Side Scope   where side = scopeStatement side

instance Side Statement where

    side (Execute (Exec mname op exprs)) = sideStatement
        $ Exec mname op <$> mapM sideExpression exprs

    side (ForStatement (ForCycle name range body)) = sideStatement
        $ ForCycle name <$> sideExpression range <*> side body

    side (IfStatement (If cond thenb elseb)) = sideStatement
        $ If <$> sideExpression cond
             <*> side thenb
             <*> side elseb

    side (ScopeStatement scope) = ScopeStatement <$> side scope
    side (Group as) = Group <$> traverse side as

sideStatement w = do
    (a, unzip -> (vardecls, sidecalls)) <- runWriterT w
    return $ ScopeStatement
           $ Scope (M.fromList vardecls) (Group (sidecalls `snoc` statement a))

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
