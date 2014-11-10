{-# LANGUAGE FlexibleInstances, ConstraintKinds #-}
module Sodium.Nucleus.Name where

import Control.Lens
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Sodium.Nucleus.Program.Vector

import Sodium.Util (mAsList)

class HasNameSource t where
    nameSource :: Lens' t [Name]

instance HasNameSource [Name] where
    nameSource = id

type NameStack t m = (Applicative m, MonadState t m, HasNameSource t)

namepop :: NameStack t m => m Name
namepop = (nameSource `uses` head) <* (nameSource %= tail)

checkRef :: Mask a => a -> Name -> Bool
checkRef a name' = getAny . execWriter
                 $ runReaderT (mask a) check
    where check name = do
            tell $ Any (name == name')
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
    mask  = (_PAccess . _1) mask
         >=> _PTuple mask

instance Mask Expression where
    mask  =  (_Access . _1) mask
         >=> _Fold  mask
         >=> _Call  mask
         >=> _MultiIfExpression mask

instance Mask a => Mask (ForCycle a) where
    mask  =  forLambda   mask
         >=> forArgExpr  mask
         >=> forRange    mask

instance Mask a => Mask (Lambda a) where
    mask  =  lamPatterns mask
         >=> lamAction   mask

instance Mask a => Mask (MultiIf a) where
    mask  =  multiIfLeafs mask

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

instance Mask a => Mask (Bind a) where
    mask  =  bindPattern   mask
         >=> bindStatement mask

instance Mask Func where
    mask  =  funcSig       mask
         >=> funcParams    mask
         >=> funcStatement mask

instance Mask FuncSig where
    mask  =  funcName    mask
         >=> funcParamTypes  mask
         >=> funcRetType mask
         >=> funcRetRefs mask

instance Mask Program where
    mask  =  programFuncs mask
