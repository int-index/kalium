{-# LANGUAGE TypeFamilies, RankNTypes, ConstraintKinds #-}

module Sodium.Nucleus.Recmap.Vector
    ( Recmapper
    , Recmap
    , recmapper
    , recmap
    , localizer
    , recmapped
    ) where

import qualified Data.Map as M

import Control.Monad
import Control.Lens
import Data.Monoid
import Sodium.Nucleus.Recmap
import Sodium.Nucleus.Program.Vector

data Vector

instance Recmap Vector where
    type RecmapFirst  Vector = Body
    type RecmapSecond Vector = Statement
    type RecmapLocal  Vector = Vars
    recmap_children_first rm body
        = localize rm (body ^. bodyVars)
        $ (bodyBinds . traversed . bindStatement) (recmap_second rm) body
    recmap_children_second rm = onMultiIf <=< onBody <=< onFor where
        r1 = recmap_first  rm
        r2 = recmap_second rm
        onMultiIf = _MultiIfStatement
            $ (multiIfLeafs . traversed . _2) r2 <=< multiIfElse r2
        onFor = _ForStatement (forAction r2)
        onBody = _BodyStatement r1


class Recmappable a where
    recmap :: Monad' m => Recmapper Vector m -> a -> m a

instance Recmappable Body where
    recmap = recmap_first

instance Recmappable Statement where
    recmap = recmap_second

instance Recmappable Func where
    recmap rm func
        = localize rm (fmap snd $ func ^. funcSig . funcParams . to M.fromList)
        $ funcBody (recmap rm) func

instance Recmappable Program where
    recmap rm = (programFuncs . traversed) (recmap rm)


class RecmapMk a where
    recmapper :: Monad' m => (a -> m a) -> Recmapper Vector m

instance RecmapMk Body where
    recmapper rm = mempty { recmap_parent_first  = rm }

instance RecmapMk Statement where
    recmapper rm = mempty { recmap_parent_second = rm }

instance RecmapMk Expression where
    recmapper rmExpr = mempty { recmap_parent_first  = rmBody
                              , recmap_parent_second = rmStatement }
        where rmStatement
                     =  _Assign rmExpr
                    >=> (_Execute . _2 . traversed) rmExpr
                    >=> _ForStatement     rmForCycle
                    >=> _MultiIfStatement rmMultiIf
              rmBody = bodyResult rmExpr
              rmForCycle = forArgExpr rmExpr >=> forRange rmExpr
              rmMultiIf = (multiIfLeafs . traversed . _1) rmExpr

localizer :: Monad' m => (forall a . Vars -> m a -> m a) -> Recmapper Vector m
localizer lc = mempty { localize = lc }

-- Be careful! Not a valid setter:
-- over recmapped f . over recmapped g /= over recmapped (f . g)
recmapped :: (Monad' m, Recmappable t, RecmapMk a) => LensLike' m t a
recmapped = recmap . recmapper
