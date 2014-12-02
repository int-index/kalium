module Sodium.Nucleus.Vector.Match where

import Control.Lens

import Sodium.Nucleus.Vector.Program
import Sodium.Nucleus.Vector.Recmap

match :: Program -> Program
match = over recmapped (bindToApp)

bindToApp :: Expression -> Expression
bindToApp = \case
    App2 (OpAccess OpBind) (OpAccess OpTaint `App` a) x -> App x a
    e -> e
