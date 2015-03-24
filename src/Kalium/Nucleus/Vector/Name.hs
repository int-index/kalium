{-# LANGUAGE FlexibleInstances #-}
module Kalium.Nucleus.Vector.Name where

import Kalium.Prelude
import Kalium.Util

import qualified Data.Map as M
import qualified Data.Set as S

import Kalium.Nucleus.Vector.Program
import Kalium.Nucleus.Vector.Pattern (patBound)

class Mentionable a where mentionable :: a -> Set Name
instance Mentionable (Set Name) where mentionable = id
instance Mentionable      Name  where mentionable = S.singleton

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

instance Mask Expression where
    mask  = \case
        Lambda pat a -> Lambda <$> mask pat <*> mask a
        Beta a1 a2 -> Beta <$> mask a1 <*> mask a2
        Primary lit -> return (Primary lit)
        Access name -> Access <$> mask name
        Ext ext -> absurd (getConst ext)

instance Mask Func where
    mask  =  funcType mask
         >=> funcExpression mask

instance Mask Program where
    mask  =  (programFuncs . mAsList) mask

alphaRename :: Map Name Name -> Expression -> Expression
alphaRename m e@(Access name) = maybe e Access (M.lookup name m)
alphaRename _ e@(Primary _) = e
alphaRename m (Beta f a) = Beta (alphaRename m f) (alphaRename m a)
alphaRename m (Lambda p a)
    = let m' = S.foldr (\name fn -> M.delete name . fn) id (patBound p) m
      in Lambda p (alphaRename m' a)
alphaRename _ (Ext ext) = absurd (getConst ext)
