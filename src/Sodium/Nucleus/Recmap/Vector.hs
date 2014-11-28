{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Sodium.Nucleus.Recmap.Vector
    ( Recmapper
    , Recmap
    , recmapper
    , recmap
    , recmapped
    , recExpr
    ) where

import Control.Applicative
import Control.Monad
import qualified Data.Map as M
import Control.Lens hiding (Fold)
import Data.Monoid
import Sodium.Nucleus.Recmap
import Sodium.Nucleus.Program.Vector

data Vector

instance Recmap Vector where
    type RecmapFirst  Vector = Body Statement
    type RecmapSecond Vector = Statement
    type RecmapLocal  Vector = ()
    recmap_children_first rm body
        = localize rm () $ (onBinds <=< onResult) body where
        onBinds = (bodyBinds . traversed . bindStatement) (recmap_second rm)
        onResult = bodyResult (recmap_second rm)
    recmap_children_second rm = onMultiIf <=< onBody <=< onFor <=< onLam where
        r1 = recmap_first  rm
        r2 = recmap_second rm
        onMultiIf = _MultiIfStatement $ (multiIfLeafs . traversed . _2) r2
        onFor = _ForStatement (forStatement r2)
        onBody = _BodyStatement r1
        onLam = _LambdaStatement (lamAction r2)


class Recmappable a where
    recmap :: Monad' m => Recmapper Vector m -> a -> m a

instance Recmappable (Body Statement) where
    recmap = recmap_first

instance Recmappable Statement where
    recmap = recmap_second

instance Recmappable Func where
    recmap rm = funcStatement (recmap rm)

instance Recmappable Program where
    recmap rm = (programFuncs . traversed) (recmap rm)


class RecmapMk a where
    recmapper :: Monad' m => (a -> m a) -> Recmapper Vector m

instance RecmapMk (Body Statement) where
    recmapper rm = mempty { recmap_parent_first  = rm }

instance RecmapMk Statement where
    recmapper rm = mempty { recmap_parent_second = rm }

instance RecmapMk Expression where
    recmapper rmExpr' = mempty { recmap_parent_second = rmStatement }
        where rmExpr = recExpr rmExpr'
              rmStatement
                     =  _Assign rmExpr
                    >=> (_Execute . _2 . traversed) rmExpr
                    >=> _ForStatement     rmForCycle
                    >=> _MultiIfStatement rmMultiIf
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
    where rf = recExpr f

-- Be careful! Not a valid setter:
-- over recmapped f . over recmapped g /= over recmapped (f . g)
recmapped :: (Monad' m, Recmappable t, RecmapMk a) => LensLike' m t a
recmapped = recmap . recmapper
