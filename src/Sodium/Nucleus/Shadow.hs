{-# LANGUAGE FlexibleContexts, ConstraintKinds, NoMonomorphismRestriction #-}
module Sodium.Nucleus.Shadow (unshadow) where

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens

import Sodium.Nucleus.Pattern
import Sodium.Nucleus.Program.Vector

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
         >=> multiIfElse unsh

instance Unshadow ForCycle where
    unsh = execStateT $ do
        let getBound = liftA2 S.union
                (uses forName S.singleton)
                (uses forArgPattern (S.fromList . patBound))
        scope <- ask
        bound <- getBound
        let shadow = mask' (scope `S.intersection` bound)
        forName %= shadow
        forArgPattern %= shadow
        forAction %= shadow

        bound' <- getBound
        local (S.union bound') $ unsh' forAction

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

class Mask a where
    mask :: a -> Reader (S.Set Name) a

mask' :: Mask a => S.Set Name -> a -> a
mask' shadowed a = runReader (mask a) shadowed

instance (Mask a, Mask b) => Mask (a, b) where
    mask = _1 mask >=> _2 mask

instance (Mask a, Mask b, Mask c) => Mask (a, b, c) where
    mask = _1 mask >=> _2 mask >=> _3 mask

instance Mask a => Mask [a] where
    mask = traverse mask

instance Mask Name where
    mask name = do
        shadowed <- asks (S.member name)
        let shadow = if shadowed then Shadow else id
        return (shadow name)

instance Mask Pattern where
    mask  = (_PAccess . _1) mask
         >=> _PTuple mask

instance Mask Expression where
    mask  =  (_Access . _1) mask
         >=> _Tuple mask
         >=> _Fold  mask
         >=> _Call  mask
         >=> _MultiIfExpression mask

instance Mask ForCycle where
    mask  =  forArgPattern mask
         >=> forArgExpr    mask
         >=> forName       mask
         >=> forRange      mask
         >=> forAction     mask

instance Mask a => Mask (MultiIf a) where
    mask  =  multiIfLeafs mask
         >=> multiIfElse  mask

instance Mask Statement where
    mask  =  _Assign  mask
         >=> _Execute mask
         >=> _ForStatement mask
         >=> _MultiIfStatement mask
         >=> _BodyStatement mask

instance Mask Body where
    mask  = (bodyVars . mAsList . traversed . _1) mask
         >=> bodyBinds  mask
         >=> bodyResult mask

instance Mask Bind where
    mask  =  bindPattern   mask
         >=> bindStatement mask

mAsList = iso M.toList M.fromList
