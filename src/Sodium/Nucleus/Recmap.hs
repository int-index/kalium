{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Sodium.Nucleus.Recmap
    ( Recmapper
    , recmapper
    , recmap
    , recmapped
    , recExpr
    ) where

import Control.Applicative
import Control.Monad
import Control.Lens hiding (Fold)
import Data.Monoid
import Data.Function
import Sodium.Nucleus.Program.Vector

type Monad' a = (Applicative a, Monad a)

type RecmapSig h = Monad' m => Recmapper m -> h -> m h

recmap_first  :: RecmapSig (Body Statement)
recmap_first  rm = recmap_parent_first  rm <=< recmap_children_first  rm

recmap_second :: RecmapSig Statement
recmap_second rm = recmap_parent_second rm <=< recmap_children_second rm

data Recmapper m = Recmapper
    { recmap_parent_first  :: Body Statement -> m (Body Statement)
    , recmap_parent_second :: Statement -> m Statement
    }

instance Monad m => Monoid (Recmapper m) where
    mappend m1 m2 = Recmapper
        { recmap_parent_first  = on (<=<) recmap_parent_first  m1 m2
        , recmap_parent_second = on (<=<) recmap_parent_second m1 m2
        }
    mempty = Recmapper return return

recmap_children_first  :: RecmapSig (Body Statement)
recmap_children_first rm = onBinds <=< onResult where
    onBinds = (bodyBinds . traversed . bindStatement) (recmap_second rm)
    onResult = bodyResult (recmap_second rm)

recmap_children_second :: RecmapSig Statement
recmap_children_second rm = \case
    Assign a  -> return (Assign  a)
    Execute a -> return (Execute a)
    MultiIfStatement a -> MultiIfStatement <$> (multiIfLeafs . traversed . _2) r2 a
    ForStatement     a -> ForStatement <$> forStatement r2 a
    BodyStatement    a -> BodyStatement <$> r1 a
    LambdaStatement  a -> LambdaStatement <$> lamAction r2 a
    where r1 = recmap_first  rm
          r2 = recmap_second rm


class Recmappable a where
    recmap :: Monad' m => Recmapper m -> a -> m a

instance Recmappable (Body Statement) where
    recmap = recmap_first

instance Recmappable Statement where
    recmap = recmap_second

instance Recmappable Func where
    recmap rm = funcStatement (recmap rm)

instance Recmappable Program where
    recmap rm = (programFuncs . traversed) (recmap rm)


class RecmapMk a where
    recmapper :: Monad' m => (a -> m a) -> Recmapper m

instance RecmapMk (Body Statement) where
    recmapper rm = mempty { recmap_parent_first  = rm }

instance RecmapMk Statement where
    recmapper rm = mempty { recmap_parent_second = rm }

instance RecmapMk Expression where
    recmapper rmExpr' = mempty { recmap_parent_second = rmStatement }
        where rmExpr = recExpr rmExpr'
              rmStatement = \case
                Assign  a -> Assign  <$> rmExpr a
                Execute a -> Execute <$> rmExpr a
                ForStatement     a -> ForStatement <$> rmForCycle a
                MultiIfStatement a -> MultiIfStatement <$> rmMultiIf a
                BodyStatement   a -> return (BodyStatement a)
                LambdaStatement a -> return (LambdaStatement a)
              rmForCycle = forArgExpr rmExpr >=> forRange rmExpr
              rmMultiIf = (multiIfLeafs . traversed . _1) rmExpr

recExpr :: Monad' m => LensLike' m Expression Expression
recExpr f = \case
    e@(Access  _) -> f e
    e@(Primary _) -> f e
    Call expr1 expr2 -> (Call <$> rf expr1 <*> rf expr2) >>= f
    MultiIfExpression multiIf ->
        (MultiIfExpression <$> (multiIfLeafs . traversed . both) rf multiIf)
        >>= f
    BodyExpression body ->
        (BodyExpression <$> ((bodyBinds . traversed . bindStatement) rf >=> bodyResult rf) body)
    where rf = recExpr f

-- Be careful! Not a valid setter:
-- over recmapped f . over recmapped g /= over recmapped (f . g)
recmapped :: (Monad' m, Recmappable t, RecmapMk a) => LensLike' m t a
recmapped = recmap . recmapper
