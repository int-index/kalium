module Kalium.Error.Insane
    ( ErrorInsane(..)
    ) where

import Prelude
import Control.Exception

data ErrorInsane = ErrorInsane String

instance Exception ErrorInsane

instance Show ErrorInsane where
    showsPrec n (ErrorInsane msg)
        = showsPrec n ("Sanity check failure: " ++ msg)
