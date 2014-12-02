module Sodium.Nucleus.Vector.Recmap where

import Control.Applicative
import Control.Monad
import Control.Lens
import Sodium.Nucleus.Vector.Program

-- Be careful! Not a valid setter:
-- over recmapped f . over recmapped g /= over recmapped (f . g)

recmap :: (Applicative m, Monad m) => LensLike' m Expression Expression
recmap outer = around where
    around = outer <=< inner
    inner = \case
        Atom a -> pure (Atom a)
        Lambda pat a -> liftA2 Lambda (pure pat) (around a)
        App a1 a2 -> liftA2 App (around a1) (around a2)

class Recmappable a where
    recmapped :: (Applicative m, Monad m) => LensLike' m a Expression

instance Recmappable Expression where
    recmapped = recmap

instance Recmappable Func where
    recmapped outer = funcExpression (recmapped outer)

instance Recmappable Program where
    recmapped outer = (programFuncs . traversed) (recmapped outer)
