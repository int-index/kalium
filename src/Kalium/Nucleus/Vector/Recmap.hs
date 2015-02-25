module Kalium.Nucleus.Vector.Recmap where

import Kalium.Prelude
import Kalium.Nucleus.Vector.Program

-- Be careful! Not a valid setter:
-- over recmapped f . over recmapped g /= over recmapped (f . g)

recmap :: (Applicative m, Monad m) => LensLike' m Expression Expression
recmap outer = around where
    around = outer <=< inner
    inner = \case
        Primary a -> pure (Primary a)
        Access  a -> pure (Access  a)
        Lambda pat a -> liftA2 Lambda (pure pat) (around a)
        Beta a1 a2 -> liftA2 Beta (around a1) (around a2)

class Recmappable a where
    recmapped :: (Applicative m, Monad m) => LensLike' m a Expression

instance Recmappable Expression where
    recmapped = recmap

instance Recmappable Func where
    recmapped outer = funcExpression (recmapped outer)

instance Recmappable Program where
    recmapped outer = (programFuncs . traversed) (recmapped outer)
