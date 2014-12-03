{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
module Sodium.Nucleus.Vector.Match where

import Control.Lens

import Sodium.Nucleus.Vector.Program
import Sodium.Nucleus.Vector.Recmap
import Sodium.Nucleus.Vector.Pattern
import Sodium.Nucleus.Vector.Name

match :: Program -> Program
match = over recmapped (matchExpression . lambdaReduce . etaReduce)

matchExpression :: Expression -> Expression
matchExpression = \case

    App (OpAccess OpId) a -> a

    App2 (OpAccess OpBind) (OpAccess OpTaint `App` a) x -> App x a

    App2 (OpAccess OpBind) x (Lambda PWildCard a)
        -> App2 (OpAccess OpBindIgnore) x a

    App2 (OpAccess OpBindIgnore) x (OpAccess OpTaint `App` a)
        -> App2 (OpAccess OpFmapIgnore) a x

    App2 (OpAccess OpFmapIgnore) (Literal STypeUnit ()) x
        -> App (OpAccess OpIgnore) x

    App2 (OpAccess OpFmapIgnore) a (App (OpAccess OpTaint) _)
        -> App (OpAccess OpTaint) a

    App2 (OpAccess OpFmapIgnore) a (App (OpAccess OpIgnore) x)
        -> App2 (OpAccess OpFmapIgnore) a x

    App (OpAccess OpIgnore) (App (OpAccess OpTaint) _)
        -> App (OpAccess OpTaint) (Literal STypeUnit ())

    App2 (OpAccess OpBind) x (OpAccess OpTaint) -> x

    App (OpAccess OpIgnore) e | isTaintedUnit e -> e
    App (OpAccess OpIgnore) (propagateOpIgnore -> e) -> e

    e -> e

-- TODO: typecheck
isTaintedUnit :: Expression -> Bool
isTaintedUnit = \case
    OpAccess OpPutLn  `App` _ -> True
    OpAccess OpIgnore `App` _ -> True
    _ -> False

etaReduce = \case
    Lambda pat (App x a)
        | preciseMatch pat a
        , not (x `mentions` patBound pat)
        -> x
    e -> e

propagateOpIgnore = \case
    App2 (OpAccess OpBind) x (Lambda p a)
        -> App2 (OpAccess OpBind) x (Lambda p (propagateOpIgnore a))
    App2 (OpAccess OpFmapIgnore) _ a -> propagateOpIgnore a
    e -> OpAccess OpIgnore `App` e

lambdaReduce = \case
    Lambda PWildCard a `App` _ -> a
    Lambda pat a | p <- cleanPattern a pat -> Lambda p a
    e -> e

cleanPattern :: Mask scope => scope -> Pattern -> Pattern
cleanPattern a = go where
    go = \case
        PUnit -> PWildCard
        PTuple p1 p2 -> case PTuple (go p1) (go p2) of
            PTuple PWildCard PWildCard -> PWildCard
            p -> p
        PAccess name _ | not (a `mentions` name) -> PWildCard
        p -> p
