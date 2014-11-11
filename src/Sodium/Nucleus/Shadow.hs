{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
module Sodium.Nucleus.Shadow (unshadow) where

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens

import Sodium.Nucleus.Pattern
import Sodium.Nucleus.Name
import Sodium.Nucleus.Program.Vector

import Sodium.Util (mAsList)

unshadow :: Program -> Program
unshadow program = program & programFuncs . traversed . funcStatement %~ unshadow'
    where unshadow' statement = runReader (unsh statement) initScope
          initScope = S.fromList (program ^.. programFuncs . traversed . funcSig . funcName)

type UnshadowScope m = (Applicative m, MonadReader (S.Set Name) m)

class Unshadow a where
    unsh :: UnshadowScope m => a -> m a

unsh' :: (Unshadow a, UnshadowScope m, MonadState s m)
      => Setting' (->) s a -> m ()
unsh' setter = do
    scope <- ask
    setter %= (\a -> runReader (unsh a) scope)

instance Unshadow Statement where
    unsh =  _ForStatement  unsh
        >=> _BodyStatement unsh
        >=> _MultiIfStatement unsh

instance Unshadow a => Unshadow (MultiIf a) where
    unsh  = (multiIfLeafs . traversed . _2) unsh

instance (Mask a, Unshadow a) => Unshadow (ForCycle a) where
    unsh = execStateT $ do
        let getBound = S.unions <$> uses
                (forLambda . lamPatterns)
                (map (S.fromList . patBound))
        scope <- ask
        bound <- getBound
        let shadow = mask' (scope `S.intersection` bound)
        forLambda %= shadow

        bound' <- getBound
        local (S.union bound') $ unsh' (forLambda . lamAction)

instance Unshadow Body where
    unsh = execStateT $ do
        let getBound = uses bodyVars (S.fromList . M.keys)
        scope <- ask
        bound <- getBound
        let shadow = mask' (scope `S.intersection` bound)
        bodyVars . mAsList . traversed . _1 %= shadow
        bodyBinds  %= shadow
        bodyResult %= shadow

        bound' <- getBound
        local (S.union bound') $ unsh' (bodyBinds . traversed . bindStatement)

mask' :: Mask a => S.Set Name -> a -> a
mask' shadowed a = runIdentity (runReaderT (mask a) (Identity . handle))
    where handle name | S.member name shadowed = NameSpace "shadow" name
                      | otherwise = name
