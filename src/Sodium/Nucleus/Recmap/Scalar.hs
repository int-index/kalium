{-# LANGUAGE TypeFamilies, RankNTypes, ConstraintKinds #-}

module Sodium.Nucleus.Recmap.Scalar
    ( Recmapper
    , Recmap
    , recmapper
    , recmap
    , localizer
    , recmapped
    ) where

import Control.Monad
import Control.Lens
import Data.Monoid
import Sodium.Nucleus.Recmap
import Sodium.Nucleus.Program.Scalar

data Scalar

instance Recmap Scalar where
    type RecmapFirst  Scalar = Body
    type RecmapSecond Scalar = Statement
    type RecmapLocal  Scalar = Vars
    recmap_children_first rm body
        = localize rm (body ^. bodyVars)
        $ (bodyStatements . traversed) (recmap_second rm) body
    recmap_children_second rm = onMultiIf <=< onBody <=< onFor where
        rr = recmap_first rm
        onMultiIf = _MultiIfStatement
            $ (multiIfLeafs . traversed . _2) rr <=< multiIfElse rr
        onFor = _ForStatement (forBody rr)
        onBody = _BodyStatement rr


class Recmappable a where
    recmap :: Monad' m => Recmapper Scalar m -> a -> m a

instance Recmappable Body where
    recmap = recmap_first

instance Recmappable Statement where
    recmap = recmap_second

instance Recmappable Func where
    recmap rm func
        = localize rm (func ^. funcSig . funcParams)
        $ funcBody (recmap rm) func

instance Recmappable Program where
    recmap rm = (programFuncs . traversed) (recmap rm)


class RecmapMk a where
    recmapper :: Monad' m => (a -> m a) -> Recmapper Scalar m

instance RecmapMk Body where
    recmapper rm = mempty { recmap_parent_first  = rm }

instance RecmapMk Statement where
    recmapper rm = mempty { recmap_parent_second = rm }


localizer :: Monad' m => (forall a . Vars -> m a -> m a) -> Recmapper Scalar m
localizer lc = mempty { localize = lc }

-- Be careful! Not a valid setter:
-- over recmapped f . over recmapped g /= over recmapped (f . g)
recmapped :: (Monad' m, Recmappable t, RecmapMk a) => LensLike' m t a
recmapped = recmap . recmapper
