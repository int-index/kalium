{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
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

transgate = transform . propagate

commonReduce :: Endo' Expression
commonReduce = commonReduce' . \case

    AppOp3 OpIf xElse xThen cond
        | AppOp1 opElse aElse <- xElse
        , AppOp1 opThen aThen <- xThen
        , opElse == opThen
        -- some type-preserving functions:
        , opElse `elem` [OpPutLn, OpPut, OpTaint]
        -- TODO: typecheck (typeof aElse == typeof aThen)
        -> AppOp1 opElse (AppOp3 OpIf aElse aThen cond)

    AppOp2 OpFmap f a
        | Just a' <- tainting propagate (Just . App1 f) a
        -> a'

    e -> e
  where
   commonReduce' = fire
    [ AppOp1 OpId "a" := "a"
    , Lambda "p" "a" := OpAccess OpId :> constraint2 preciseMatch "p" "a"

    , Bind (Taint "a") "x" := App1 "x" "a"
    , Bind "x" (OpAccess OpTaint) := "x"

    , AppOp1 OpPutLn (AppOp1 OpShow "a") := AppOp1 OpPrintLn "a"

    , AppOp3 OpIf (Taint LitUnit) "x" "a" := AppOp2 OpWhen "a" "x"
    , AppOp2 OpWhen LitFalse "a" := Taint LitUnit
    , AppOp1 OpWhen LitTrue := OpAccess OpId
    ]

appIgnore :: Endo' Expression
appIgnore = fire
    [ Follow PWildCard "x" "a" := AppOp2 OpBindIgnore "x" "a"
    , AppOp2 OpBindIgnore (Taint LitUnit) "a" := "a"
    , AppOp2 OpBindIgnore "x" (Taint "a") := AppOp2 OpFmapIgnore "a" "x"

    , AppOp2 OpFmapIgnore LitUnit "x" := Ignore "x"
    , AppOp2 OpFmapIgnore "a" (Taint  "x") := Taint "a"
    , AppOp2 OpFmapIgnore "a" (Ignore "x") := AppOp2 OpFmapIgnore "a" "x"

    , AppOp2 OpBindIgnore "x" "a"
        := AppOp2 OpBindIgnore "x" "a"
        :> transgate attemptIgnore "x"

    , AppOp2 OpFmapIgnore "a" "x"
        := AppOp2 OpFmapIgnore "a" "x"
        :> transgate attemptIgnore "x"

    , Ignore "a" := "a" :> transgate attemptIgnore "a"

    , AppOp3 OpFoldTainted (Lambda2 PWildCard "p" "a") "_1" "x"
        := AppOp2 OpMapTaintedIgnore (Lambda "p" "a") "x"
        :> transgate attemptIgnore "a"

    , AppOp2 OpFmapIgnore "a" "x" := "x'"
        .:> do
         a <- mmeta "a"
         x <- mmeta "x"
         "x'" ..= over propagate (AppOp2 OpFmapIgnore a) x
    ]

attemptIgnore :: EndoKleisli' Maybe Expression
attemptIgnore = \case
    Taint _ -> Just (Taint LitUnit)
    e | isTaintedUnit e -> Just e
    AppOp2 OpFmapIgnore _ a -> Just (Ignore a)
    AppOp2 OpFmap _ a -> Just (Ignore a)
    _ -> Nothing

foldMatch :: Endo' Expression
foldMatch = fire
    [ AppOp3 OpFoldTainted (Lambda2 "p1" "p2" (Taint "a")) "x1" "x2"
        := Taint (AppOp3 OpFold (Lambda2 "p1" "p2" "a") "x1" "x2")

    , AppOp3 OpFoldTainted (Lambda2 PWildCard "p2" "a") LitUnit "x2"
        := AppOp2 OpMapTaintedIgnore (Lambda "p2" "a") "x2"

    , AppOp3 OpFoldTainted (Lambda PWildCard "a") "x1" "x2"
        := AppOp3 OpFoldTainted (Lambda PWildCard "a") (OpAccess OpUndefined) "x2"

    , AppOp2 OpFold (OpAccess OpMultiply) LitOne := OpAccess OpProduct
    , AppOp2 OpFold (OpAccess OpAdd) LitZero := OpAccess OpSum
    , AppOp2 OpFold (OpAccess OpAnd) LitTrue := OpAccess OpAnd'
    , AppOp2 OpFold (OpAccess OpOr) LitFalse := OpAccess OpOr'
    ]

leftIdentity  f x = App2 f x "a" := "a"
rightIdentity f x = App2 f "a" x := "a"

unaryIdempotence  f = App1 f (App1 f "a") := "a"
binaryIdempotence f = App2 f "a" "a" := "a"

booleanCompute :: Endo' Expression
booleanCompute = fire
    [ AppOp1 OpNot LitTrue  := LitFalse
    , AppOp1 OpNot LitFalse := LitTrue

    , unaryIdempotence (OpAccess OpNot)
    , binaryIdempotence (OpAccess OpAnd)
    , binaryIdempotence (OpAccess OpOr)

    , AppOp2 OpAnd "x" (AppOp1 OpNot "x") := LitFalse
    , AppOp2 OpAnd (AppOp1 OpNot "x") "x" := LitFalse

    , AppOp2 OpOr "x" (AppOp1 OpNot "x") := LitTrue
    , AppOp2 OpOr (AppOp1 OpNot "x") "x" := LitTrue

    , leftIdentity  (OpAccess OpAnd) LitTrue
    , rightIdentity (OpAccess OpAnd) LitTrue

    , leftIdentity  (OpAccess OpOr) LitFalse
    , rightIdentity (OpAccess OpOr) LitFalse

    , AppOp2 OpAnd LitFalse "x" := LitFalse
    , AppOp2 OpAnd "x" LitFalse := LitFalse

    , AppOp2 OpOr LitTrue "x" := LitTrue
    , AppOp2 OpOr "x" LitTrue := LitTrue

    , AppOp3 OpIf "x" "a" LitFalse := "x"
    , AppOp3 OpIf "a" "x" LitTrue  := "x"
    ]

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
pairReduce = fire
    [ Lambda (PTuple "p1" "p2") "a" := Lambda (PTuple "p2" "p1") "a'"
       .:> do
        p1 <- mmeta "p1"
        p2 <- mmeta "p2"
        a <- mmeta "a"
        a' <- amaybe $ propagate (swapApp p1 p2) a
        "a'" ..= a'

    , AppOp1 OpSwap "a" := "a" :> transgate swapAttempt "a"
    , AppOp1 OpFst  "a" := "a" :> transgate fstAttempt "a"
    , AppOp1 OpSnd  "a" := "a" :> transgate sndAttempt "a"
    ]
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
listReduce = fire
    [ leftIdentity  (OpAccess OpConcat) (OpAccess OpNil)
    , rightIdentity (OpAccess OpConcat) (OpAccess OpNil)
    , AppOp2 OpConcat "es" "e" := "a"
       .:> do
        e <- mmeta "e"
        es <- mmeta "es"
        _   <- unlist e
        es' <- unlist es
        "a" ..= foldr (AppOp2 OpCons) e es'
    ]
  where
    unlist :: Alternative f => Expression -> f [Expression]
    unlist = \case
        AppOp2 OpCons e es -> (e:) <$> unlist es
        OpAccess OpNil -> pure []
        _ -> empty

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

notMentions :: Pattern -> Expression -> Bool
notMentions p x = not (x `mentions` patBound p)

etaReduce = fire
    [ Eta "p" "x" "a" := "x"
        :> constraint2 preciseMatch "p" "a"
        :> constraint2  notMentions "p" "x"
    ]

lambdaReduce = fire
    [ Into PWildCard "x" "a" := "a"
    , Into "p" "x" (Taint "a") := Taint (Into "p" "x" "a")
    , Lambda "p" "a" := Lambda "p'" "a"
       .:> do
        p <- mmeta "p"
        a <- mmeta "a"
        "p'" ..= cleanPattern (a :: Expression) p
    ]

cleanPattern :: Mask scope => scope -> Pattern -> Pattern
cleanPattern a = go where
    go = \case
        PUnit -> PWildCard
        PTuple p1 p2 -> case PTuple (go p1) (go p2) of
            PTuple PWildCard PWildCard -> PWildCard
            p -> p
        PAccess name _ | not (a `mentions` name) -> PWildCard
        p -> p
