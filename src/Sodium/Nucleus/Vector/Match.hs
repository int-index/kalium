module Sodium.Nucleus.Vector.Match where

import Control.Lens

import Sodium.Nucleus.Vector.Program
import Sodium.Nucleus.Vector.Recmap
import Sodium.Nucleus.Vector.Name

match :: Program -> Program
match = over recmapped (bindToApp . lamClean . lamReduce)

bindToApp :: Expression -> Expression
bindToApp = \case
    App2 (OpAccess OpBind) (OpAccess OpTaint `App` a) x -> App x a
    e -> e

cleanUsage :: Mask scope => scope -> Pattern -> Pattern
cleanUsage a = go where
    go = \case
        PUnit -> PWildCard
        PTuple p1 p2 -> PTuple (go p1) (go p2)
        PAccess name _ | not (checkRef a name) -> PWildCard
        p -> p

lamClean :: Expression -> Expression
lamClean = \case
    Lambda pat a -> Lambda (cleanUsage a pat) a
    e -> e

lamReduce :: Expression -> Expression
lamReduce = \case
    Lambda PWildCard a `App` _ -> a
    e -> e
