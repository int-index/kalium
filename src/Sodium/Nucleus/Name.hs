{-# LANGUAGE FlexibleInstances, ConstraintKinds #-}
module Sodium.Nucleus.Name where

import Control.Lens
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Sodium.Nucleus.Program.Vector

import Sodium.Util (mAsList)

class HasNameSource t where
    nameSource :: Lens' t [Name]

instance HasNameSource [Name] where
    nameSource = id

type NameStack t m = (Applicative m, MonadState t m, HasNameSource t)

namepop :: NameStack t m => m Name
namepop = (nameSource `uses` head) <* (nameSource %= tail)


class Mask a where
    mask :: (Applicative m, Monad m) => a -> ReaderT (Name -> m Name) m a

instance (Mask a, Mask b) => Mask (a, b) where
    mask = _1 mask >=> _2 mask

instance (Mask a, Mask b, Mask c) => Mask (a, b, c) where
    mask = _1 mask >=> _2 mask >=> _3 mask

instance Mask a => Mask [a] where
    mask = traverse mask

instance Mask Name where
    mask name = do
        k <- ask
        lift (k name)

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
