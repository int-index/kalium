{-# LANGUAGE DataKinds #-}
module Sodium.Nucleus.Pass.Inline (inline) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Lens hiding (Index, Fold)
import qualified Data.Map as M
import Sodium.Nucleus.Program.Vector
import Sodium.Nucleus.Recmap.Vector
import Sodium.Nucleus.Pattern
import Data.Bool

inline :: Program -> Program
inline = over recmapped inlineBody

inlineBody :: Body -> Body
inlineBody = evalState unconsBind where
    unconsBind :: State Body Body
    unconsBind = uses bodyBinds uncons >>= maybe get go
    go (bind, binds) = do
        bodyBinds .= binds
        elim bind <$> get >>= \case
            Just body -> put body >> unconsBind
            Nothing   -> over bodyBinds (bind:) <$> unconsBind
    elim bind body' = do
        Bind (PAccess name i) (Assign expr) <- return bind
        let (body, count)
              = runWriter
              $ flip runReaderT ((name, i), expr)
              $ unsafeBodySubOnce body'
        guard (count <= 1)
        return body

type SubOnceEnv = ((Name, Index), Expression)

class SubOnce a where
	subOnce :: a -> ReaderT SubOnceEnv (Writer (Sum Integer)) a

apUnless :: Monad m => (a -> m Bool) -> (a -> m a) -> (a -> m a)
apUnless p f = \a -> p a >>= bool (f a) (return a)

instance SubOnce Expression where
    subOnce = \case
        Primary prim -> return (Primary prim)
        Access name' j -> do
            (name, expr) <- ask
            if name == (name', j)
                then tell (Sum 1) >> return expr
                else return (Access name' j)
        Tuple exprs
             -> Tuple
            <$> traversed subOnce exprs
        Call op exprs
             -> Call op
            <$> traversed subOnce exprs
        Fold op expr range
             -> Fold op
            <$> subOnce expr
            <*> subOnce range

instance SubOnce Statement where
	subOnce
		 = (_Execute . _2 . traversed) subOnce
		>=> _Assign subOnce
		>=> _BodyStatement    subOnce
		>=> _ForStatement     subOnce
		>=> _MultiIfStatement subOnce

instance SubOnce Bind where
	subOnce = bindStatement subOnce

instance SubOnce ForCycle where
	subOnce
		 =  forRange subOnce
		>=> forArgExpr subOnce
		>=> apUnless
			(shadowedBy . view (forArgPattern . to patBound))
			(forAction subOnce)

instance SubOnce MultiIfBranch where
	subOnce
		 = (multiIfLeafs . traversed) (_1 subOnce >=> _2 subOnce)
		>=> multiIfElse subOnce

instance SubOnce Body where
	subOnce = apUnless
		(shadowedBy . M.keys . view bodyVars)
		unsafeBodySubOnce

unsafeBodySubOnce
	 =  (bodyBinds   . traversed) subOnce
	>=> bodyResult subOnce

shadowedBy :: Monad m => [Name] -> ReaderT SubOnceEnv m Bool
shadowedBy names = do
	(name, _) <- ask
	return $ fst name `elem` names
