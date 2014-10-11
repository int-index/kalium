{-# LANGUAGE FlexibleInstances, ConstraintKinds #-}
module Sodium.Nucleus.Name where

import Control.Lens
import Control.Applicative
import Control.Monad.State
import Sodium.Nucleus.Program

class HasNameSource t where
    nameSource :: Lens' t [Name]

instance HasNameSource [Name] where
    nameSource = id

type NameStack t m = (Applicative m, MonadState t m, HasNameSource t)

namepop :: NameStack t m => m Name
namepop = (nameSource `uses` head) <* (nameSource %= tail)
