{-# LANGUAGE FlexibleContexts #-}
module Kalium.Nucleus.Vector.Match where

import Kalium.Prelude

import Kalium.Nucleus.Vector.Program
import Kalium.Nucleus.Vector.Recmap
import Kalium.Nucleus.Vector.Pattern
import Kalium.Nucleus.Vector.Name
import Kalium.Nucleus.Vector.Attempt
import Kalium.Nucleus.Vector.Template
import Kalium.Util

match :: Endo' Program
match = over recmapped
      $ commonReduce
      . lambdaReduce
      . etaReduce
      . pairReduce
      . listReduce
      . foldMatch
      . booleanCompute
      . numericCompute
      . appIgnore

pattern LitZero = Primary (LitInteger 0)
pattern LitOne  = Primary (LitInteger 1)
pattern LitTrue = OpAccess OpTrue
pattern LitFalse = OpAccess OpFalse

commonReduce :: Endo' Expression
commonReduce = \case

    AppOp1 OpId a -> a
    Lambda p a | preciseMatch p a -> OpAccess OpId

    Bind (Taint a) x -> App1 x a
    Bind x (OpAccess OpTaint) -> x

    AppOp1 OpPutLn (AppOp1 OpShow a) -> AppOp1 OpPrintLn a

    AppOp3 OpIf (Taint LitUnit) xThen cond -> AppOp2 OpWhen cond xThen
    AppOp2 OpWhen LitTrue a -> a
    AppOp2 OpWhen LitFalse _ -> Taint LitUnit

    AppOp3 OpIf xElse xThen cond
        | AppOp1 opElse aElse <- xElse
        , AppOp1 opThen aThen <- xThen
        , opElse == opThen
        -- some type-preserving functions:
        , opElse `elem` [OpPutLn, OpPut, OpTaint]
        -- TODO: typecheck (typeof aElse == typeof aThen)
        -> AppOp1 opElse (AppOp3 OpIf aElse aThen cond)

    e -> e

appIgnore :: Endo' Expression
appIgnore = \case

    Follow PWildCard x a -> AppOp2 OpBindIgnore x a
    AppOp2 OpBindIgnore (Taint LitUnit) a -> a
    AppOp2 OpBindIgnore x (Taint a) -> AppOp2 OpFmapIgnore a x
    AppOp2 OpBindIgnore (propagate attemptIgnore -> Just x) a
        -> AppOp2 OpBindIgnore x a

    AppOp2 OpFmapIgnore LitUnit x -> Ignore x
    AppOp2 OpFmapIgnore a (Taint  _) -> Taint a
    AppOp2 OpFmapIgnore a (Ignore x) -> AppOp2 OpFmapIgnore a x
    AppOp2 OpFmapIgnore a (propagate attemptIgnore -> Just e)
        -> AppOp2 OpFmapIgnore a e
    AppOp2 OpFmapIgnore c (over propagate (AppOp2 OpFmapIgnore c) -> e) -> e

    AppOp3 OpFoldTainted (Lambda2 PWildCard p2 a) _ x2
         | Just a' <- propagate attemptIgnore a
        -> AppOp2 OpMapTaintedIgnore (Lambda p2 a') x2

    Ignore (propagate attemptIgnore -> Just e) -> e

    e -> e

attemptIgnore :: EndoKleisli' Maybe Expression
attemptIgnore = \case
    Taint _ -> Just (Taint LitUnit)
    e | isTaintedUnit e -> Just e
    AppOp2 OpFmapIgnore _ a -> Just (Ignore a)
    _ -> Nothing

foldMatch :: Endo' Expression
foldMatch = fire
    [ AppOp3 OpFoldTainted (Lambda2 p1 p2 (Taint a)) x1 x2
        := Taint (AppOp3 OpFold (Lambda2 p1 p2 a) x1 x2)

    , AppOp3 OpFoldTainted (Lambda2 PWildCard p2 a) LitUnit x2
        := AppOp2 OpMapTaintedIgnore (Lambda p2 a) x2

    , AppOp3 OpFoldTainted (Lambda PWildCard a) x1 x2
        := AppOp3 OpFoldTainted (Lambda PWildCard a) (OpAccess OpUndefined) x2

    , AppOp3 OpFold (OpAccess OpMultiply) LitOne a := AppOp1 OpProduct a
    , AppOp3 OpFold (OpAccess OpAdd) LitZero a := AppOp1 OpSum a
    , AppOp3 OpFold (OpAccess OpAnd) LitTrue a := AppOp1 OpAnd' a
    , AppOp3 OpFold (OpAccess OpOr) LitFalse a := AppOp1 OpOr' a
    ] where (x1:x2:a:_, p1:p2:_) = metaSource2

booleanCompute :: Endo' Expression
booleanCompute = fire
    [ AppOp1 OpNot LitTrue  := LitFalse
    , AppOp1 OpNot LitFalse := LitTrue

    , AppOp2 OpAnd x x := x
    , AppOp2 OpOr  x x := x

    , AppOp2 OpAnd x (AppOp1 OpNot x) := LitFalse
    , AppOp2 OpAnd (AppOp1 OpNot x) x := LitFalse

    , AppOp2 OpOr x (AppOp1 OpNot x) := LitTrue
    , AppOp2 OpOr (AppOp1 OpNot x) x := LitTrue

    , AppOp2 OpAnd LitTrue x := x
    , AppOp2 OpAnd x LitTrue := x

    , AppOp2 OpOr x LitFalse := x
    , AppOp2 OpOr LitFalse x := x

    , AppOp2 OpAnd LitFalse x := LitFalse
    , AppOp2 OpAnd x LitFalse := LitFalse

    , AppOp2 OpOr LitTrue x := LitTrue
    , AppOp2 OpOr x LitTrue := LitTrue

    , AppOp3 OpIf x a LitFalse := x
    , AppOp3 OpIf a x LitTrue  := x
    ] where (x:a:_) = metaSource

numericCompute :: Endo' Expression
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

integerOp2 :: NameSpecial -> Maybe (Integer -> Integer -> Integer)
integerOp2 = \case
    OpAdd -> return (+)
    OpSubtract -> return (-)
    OpMultiply -> return (*)
    OpDiv -> return div
    OpMod -> return mod
    _ -> Nothing

doubleOp2 :: NameSpecial -> Maybe (Rational -> Rational -> Rational)
doubleOp2 = \case
    OpAdd -> return (+)
    OpSubtract -> return (-)
    OpMultiply -> return (*)
    OpDivide -> return (/)
    _ -> Nothing

pairReduce :: Endo' Expression
pairReduce = \case
    Lambda (PTuple p1 p2) (propagate (swapApp p1 p2) -> Just a)
        -> Lambda (PTuple p2 p1) a
    AppOp1 OpSwap (propagate swapAttempt -> Just a) -> a
    AppOp1 OpFst (propagate fstAttempt -> Just a) -> a
    AppOp1 OpSnd (propagate sndAttempt -> Just a) -> a
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

listReduce :: Endo' Expression
listReduce = \case
    AppOp2 OpConcat (OpAccess OpNil) e -> e
    AppOp2 OpConcat e (OpAccess OpNil) -> e

    AppOp2 OpConcat (unlist -> Just es) e@(unlist -> Just _)
        -> foldr (AppOp2 OpCons) e es
    e -> e
  where
    unlist = \case
        AppOp2 OpCons e es -> (e:) <$> unlist es
        OpAccess OpNil -> pure []
        _ -> Nothing

-- TODO: typecheck
isTaintedUnit :: Expression -> Bool
isTaintedUnit = \case
    Taint LitUnit -> True
    AppOp1 OpPutLn  _ -> True
    AppOp1 OpPut _ -> True
    AppOp1 OpPrintLn _ -> True
    AppOp1 OpIgnore _ -> True
    AppOp2 OpWhen _ _ -> True
    AppOp2 OpMapTaintedIgnore _ _ -> True
    AppOp3 OpIf xElse xThen _ -> isTaintedUnit xElse && isTaintedUnit xThen
    _ -> False

etaReduce = \case
    Eta p x a | preciseMatch p a
              , not (x `mentions` patBound p) -> x
    e -> e

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
