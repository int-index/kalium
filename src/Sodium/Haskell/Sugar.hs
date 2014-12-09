module Sodium.Haskell.Sugar where

import Language.Haskell.Exts
import Sodium.Util (closureM)

sugarcoat :: Monad m => Module -> m Module
sugarcoat = closureM sugar

class Sugar a where
    sugar :: Monad m => a -> m a

instance Sugar Module where
    sugar = return
