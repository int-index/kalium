module Sodium.Nucleus.Vector.Match where

import Control.Lens

import Sodium.Nucleus.Vector.Program
import Sodium.Nucleus.Vector.Recmap
import Sodium.Nucleus.Vector.Pattern
import Sodium.Nucleus.Vector.Name
import Sodium.Nucleus.Vector.Attempt

match :: Program -> Program
match = over recmapped
      $ matchExpression
      . lambdaReduce
      . etaReduce
      . pairReduce
      . foldMatch
      . booleanCompute
      . numericCompute

pattern LitZero = Primary (LitInteger 0)
pattern LitOne  = Primary (LitInteger 1)
pattern LitTrue = OpAccess OpTrue
pattern LitFalse = OpAccess OpFalse

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

    AppOp3 OpIf xElse xThen cond
        | AppOp1 opElse aElse <- xElse
        , AppOp1 opThen aThen <- xThen
        , opElse == opThen
        -- some type-preserving functions:
        , opElse `elem` [OpPutLn, OpTaint]
        -- TODO: typecheck (typeof aElse == typeof aThen)
        -> AppOp1 opElse (AppOp3 OpIf aElse aThen cond)

    e -> e

foldMatch :: Expression -> Expression
foldMatch = \case
    AppOp3 OpFoldTainted (Lambda2 p1 p2 (Taint a)) x1 x2
        -> Taint (AppOp3 OpFold (Lambda2 p1 p2 a) x1 x2)

    AppOp3 OpFold (OpAccess OpMultiply) LitOne x -> AppOp1 OpProduct x
    AppOp3 OpFold (OpAccess OpAdd) LitZero x -> AppOp1 OpSum x
    AppOp3 OpFold (OpAccess OpAnd) LitTrue x -> AppOp1 OpAnd' x
    AppOp3 OpFold (OpAccess OpOr) LitFalse x -> AppOp1 OpOr' x

    e -> e

booleanCompute :: Expression -> Expression
booleanCompute = \case

    AppOp1 OpNot LitTrue  -> LitFalse
    AppOp1 OpNot LitFalse -> LitTrue

    AppOp2 op x y | x == y, op == OpAnd || op == OpOr -> x

    AppOp2 OpAnd x y | x == AppOp1 OpNot y || AppOp1 OpNot x == y -> LitFalse
    AppOp2 OpOr  x y | x == AppOp1 OpNot y || AppOp1 OpNot x == y -> LitTrue

    AppOp2 OpAnd (OpAccess OpTrue) a -> a
    AppOp2 OpAnd a (OpAccess OpTrue) -> a
    AppOp2 OpOr a (OpAccess OpFalse) -> a
    AppOp2 OpOr (OpAccess OpFalse) a -> a

    AppOp2 OpAnd x y | x == LitFalse || y == LitFalse -> LitFalse
    AppOp2 OpOr  x y | x == LitTrue  || y == LitTrue  -> LitTrue

    AppOp3 OpIf xElse _ LitFalse -> xElse
    AppOp3 OpIf _ xThen LitTrue  -> xThen

    e -> e

numericCompute :: Expression -> Expression
numericCompute = \case

    AppOp2 op (Primary (LitDouble x)) (Primary (LitDouble y))
        | op == OpEquals -> if x == y then LitTrue else LitFalse
        | Just f <- doubleOp2 op -> Primary (LitDouble (f x y))

    AppOp2 op (Primary (LitInteger x)) (Primary (LitInteger y))
        | op == OpEquals -> if x == y then LitTrue else LitFalse
        | Just f <- integerOp2 op -> Primary (LitInteger (f x y))

    AppOp1 OpIntToDouble (Primary (LitInteger a))
        -> Primary (LitDouble (fromIntegral a))

    e -> e

integerOp2 :: Operator -> Maybe (Integer -> Integer -> Integer)
integerOp2 = \case
    OpAdd -> return (+)
    OpSubtract -> return (-)
    OpMultiply -> return (*)
    OpDiv -> return div
    OpMod -> return mod
    _ -> Nothing

doubleOp2 :: Operator -> Maybe (Rational -> Rational -> Rational)
doubleOp2 = \case
    OpAdd -> return (+)
    OpSubtract -> return (-)
    OpMultiply -> return (*)
    OpDivide -> return (/)
    _ -> Nothing

pairReduce :: Expression -> Expression
pairReduce = \case
    Lambda (PTuple p1 p2) (pureAttempt (swapApp p1 p2) -> Just a)
        -> Lambda (PTuple p2 p1) a
    AppOp1 OpSwap (pureAttempt swapAttempt -> Just a) -> a
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
    swapAttempt = \case
        AppOp2 OpPair a b -> Just (AppOp2 OpPair b a)
        _ -> Nothing
    swapApp p1 p2 = \case
        e@(AppOp2 OpPair a1 a2)
            | preciseMatch p1 a2
            , preciseMatch p2 a1
            -> Just (AppOp1 OpSwap e)
        _ -> Nothing

-- TODO: typecheck
isTaintedUnit :: Expression -> Bool
isTaintedUnit = \case
    Taint LitUnit -> True
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
