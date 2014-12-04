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

    AppOp1 OpId a -> a

    Bind (Taint a) x -> App1 x a
    Bind x (OpAccess OpTaint) -> x

    Follow PWildCard x a -> AppOp2 OpBindIgnore x a
    AppOp2 OpBindIgnore x (Taint a) -> AppOp2 OpFmapIgnore a x

    AppOp2 OpFmapIgnore LitUnit x -> Ignore x
    AppOp2 OpFmapIgnore a (Taint  _) -> Taint a
    AppOp2 OpFmapIgnore a (Ignore x) -> AppOp2 OpFmapIgnore a x

    Ignore (Taint _) -> Taint LitUnit
    Ignore e | isTaintedUnit e -> e
    Ignore (propagateOpIgnore -> e) -> e

    e -> e

-- TODO: typecheck
isTaintedUnit :: Expression -> Bool
isTaintedUnit = \case
    AppOp1 OpPutLn  _ -> True
    AppOp1 OpIgnore _ -> True
    _ -> False

etaReduce = \case
    Eta p x a | preciseMatch p a
              , not (x `mentions` patBound p) -> x
    e -> e

propagateOpIgnore = \case
    Follow p x a -> Follow p x (propagateOpIgnore a)
    AppOp2 OpFmapIgnore _ a -> propagateOpIgnore a
    Lambda p a `Beta` x -> Lambda p (propagateOpIgnore a) `Beta` x
    e -> Ignore e

lambdaReduce = \case
    Lambda PWildCard a `Beta` _ -> a
    Lambda p a -> Lambda (cleanPattern a p) a
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
