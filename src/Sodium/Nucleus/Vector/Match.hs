module Sodium.Nucleus.Vector.Match where

import Control.Lens

import Sodium.Nucleus.Vector.Program
import Sodium.Nucleus.Vector.Recmap
import Sodium.Nucleus.Vector.Pattern
import Sodium.Nucleus.Vector.Name
import Sodium.Nucleus.Vector.Attempt

match :: Program -> Program
match = over recmapped (matchExpression . lambdaReduce . etaReduce . pairReduce)

pattern LitZero = Primary (LitInteger 0)
pattern LitOne  = Primary (LitInteger 1)

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
    AppOp2 OpFmapIgnore c (propagateOpFmapIgnore c -> e) -> e

    Ignore (Taint _) -> Taint LitUnit
    Ignore e | isTaintedUnit e -> e
    Ignore (propagateOpIgnore -> e) -> e

    AppOp1 OpPutLn (AppOp1 OpShow a) -> AppOp1 OpPrintLn a

    AppOp3 OpFoldTainted (Lambda2 p1 p2 (Taint a)) x1 x2
        -> Taint (AppOp3 OpFold (Lambda2 p1 p2 a) x1 x2)

    AppOp3 OpFold (OpAccess OpMultiply) LitOne x -> AppOp1 OpProduct x
    AppOp3 OpFold (OpAccess OpAdd) LitZero x -> AppOp1 OpSum x
    AppOp3 OpFold (OpAccess OpAnd) (OpAccess OpTrue) x -> AppOp1 OpAnd' x
    AppOp3 OpFold (OpAccess OpOr) (OpAccess OpFalse) x -> AppOp1 OpOr' x

    AppOp3 OpIf xElse xThen cond
        | AppOp1 opElse aElse <- xElse
        , AppOp1 opThen aThen <- xThen
        , opElse == opThen
        -- some type-preserving functions:
        , opElse `elem` [OpPutLn, OpTaint]
        -- TODO: typecheck (typeof aElse == typeof aThen)
        -> AppOp1 opElse (AppOp3 OpIf aElse aThen cond)

    e -> e

pairReduce :: Expression -> Expression
pairReduce = \case
    AppOp1 OpFst (pureAttempt fstAttempt -> Just a) -> a
    AppOp1 OpSnd (pureAttempt sndAttempt -> Just a) -> a
    e -> e
  where
    fstAttempt = \case
        AppOp2 OpPair a _ -> Just a
        _ -> Nothing
    sndAttempt = \case
        AppOp2 OpPair _ a -> Just a
        _ -> Nothing

-- TODO: typecheck
isTaintedUnit :: Expression -> Bool
isTaintedUnit = \case
    AppOp1 OpPutLn  _ -> True
    AppOp1 OpPrintLn _ -> True
    AppOp1 OpIgnore _ -> True
    AppOp3 OpIf xElse xThen _ -> isTaintedUnit xElse && isTaintedUnit xThen
    _ -> False

etaReduce = \case
    Eta p x a | preciseMatch p a
              , not (x `mentions` patBound p) -> x
    e -> e

propagateOpIgnore = \case
    Follow p x a -> Follow p x (propagateOpIgnore a)
    AppOp2 OpFmapIgnore _ a -> propagateOpIgnore a
    Into p x a -> Into p x (propagateOpIgnore a)
    e -> Ignore e

propagateOpFmapIgnore c = \case
    Follow p x a -> Follow p x (propagateOpFmapIgnore c a)
    AppOp2 OpFmapIgnore _ a -> propagateOpFmapIgnore c a
    Into p x a -> Into p x (propagateOpFmapIgnore c a)
    e -> AppOp2 OpFmapIgnore c e

lambdaReduce = \case
    Into PWildCard _ a -> a
    Into p x (Taint a) -> Taint (Into p x a)
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
