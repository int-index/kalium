{-# LANGUAGE FlexibleInstances #-}
module Sodium.Nucleus.Vector.Name where

import qualified Data.Set as S
import Control.Lens
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Sodium.Nucleus.Vector.Program

class Mentionable a where mentionable :: a -> S.Set Name
instance Mentionable (S.Set Name) where mentionable = id
instance Mentionable        Name  where mentionable = S.singleton

mentions :: (Mask a, Mentionable names) => a -> names -> Bool
a `mentions` names = getAny . execWriter
                 $ runReaderT (mask a) check
    where check name = do
            tell $ Any (name `S.member` mentionable names)
            return name

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

instance Mask Type where
    -- no user-defined types yet
    mask = return

instance Mask Pattern where
    mask  =  \case
        PAccess name ty -> PAccess <$> mask name <*> mask ty
        PTuple  p1   p2 -> PTuple  <$> mask p1   <*> mask p2
        PWildCard -> return PWildCard
        PUnit     -> return PUnit

instance Mask Atom where
    mask  =  \case
        Primary lit -> return (Primary lit)
        Access name -> Access <$> mask name

instance Mask Expression where
    mask  = \case
        Atom a -> Atom <$> mask a
        Lambda pat a -> Lambda <$> mask pat <*> mask a
        App a1 a2 -> App <$> mask a1 <*> mask a2

instance Mask Func where
    mask  =  funcType mask
         >=> funcName mask
         >=> funcExpression mask

instance Mask Program where
    mask  =  programFuncs mask
