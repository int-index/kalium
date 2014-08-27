{-# LANGUAGE RankNTypes, ConstraintKinds, TypeFamilies #-}
module Sodium.Chloride.Recmap where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Function

type Monad' a = (Applicative a, Monad a)

type RecmapSig h x = Monad' m => Recmapper x m -> h x -> m (h x)

-- In order to allow better type inference, injective type families
-- are needed. See GHC ticket #6018
class Recmap x where
    type RecmapFirst  x :: *
    type RecmapSecond x :: *
    type RecmapLocal  x :: *
    recmap_children_first  :: RecmapSig RecmapFirst  x
    recmap_children_second :: RecmapSig RecmapSecond x

recmap_first  :: Recmap x => RecmapSig RecmapFirst x
recmap_first  rm = on (<=<) ($ rm) recmap_parent_first  recmap_children_first

recmap_second :: Recmap x => RecmapSig RecmapSecond x
recmap_second rm = on (<=<) ($ rm) recmap_parent_second recmap_children_second

data Recmapper x m = Recmapper
    { recmap_parent_first  :: RecmapFirst  x -> m (RecmapFirst  x)
    , recmap_parent_second :: RecmapSecond x -> m (RecmapSecond x)
    , localize :: forall a  . RecmapLocal  x -> m a -> m a
    }

instance Monad m => Monoid (Recmapper x m) where
    mappend m1 m2 = Recmapper
        { recmap_parent_first  = on (<=<) recmap_parent_first  m1 m2
        , recmap_parent_second = on (<=<) recmap_parent_second m1 m2
        , localize = on (liftA2 fmap) localize m1 m2
        }
    mempty = Recmapper return return (const id)
