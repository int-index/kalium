module Kalium.Nucleus.Vector.BindClean where

import Kalium.Prelude
import Kalium.Util

import Kalium.Nucleus.Vector.Program
import Kalium.Nucleus.Vector.Recmap
import Kalium.Nucleus.Vector.Attempt

bindClean :: Endo' Program
bindClean = over recmapped bindCleanExpression

bindCleanExpression :: Endo' Expression
bindCleanExpression = \case
    Follow p x a | Just e <- asum $ map (subst Follow tainting x a) (patClean p) -> e
    Into   p x a | Just e <- asum $ map (subst Into   id       x a) (patClean p) -> e
    e -> e

subst h t x a (p, c) = t propagate c x <&> \x' -> h p x' a

type Con = forall a . Endo' (a,a)

patClean :: Pattern -> Pairs Pattern (EndoKleisli' Maybe Expression)
patClean p@PWildCard = return (p, \_ -> return LitUnit)
patClean (PTuple PWildCard p) = return (p, Just . AppOp1 OpSnd)
patClean (PTuple p PWildCard) = return (p, Just . AppOp1 OpFst)
patClean (PTuple p1 p2) = fstClean ++ sndClean ++ swapClean where
    fstClean = do
        (p, c) <- patClean p1
        let cln (AppOp2 OpPair e1 e2) = AppOp2 OpPair <$> c e1 <*> pure e2
            cln _ = Nothing
        return (PTuple p p2, cln)
    sndClean = do
        (p, c) <- patClean p2
        let cln (AppOp2 OpPair e1 e2) = AppOp2 OpPair <$> pure e1 <*> c e2
            cln _ = Nothing
        return (PTuple p1 p, cln)
    swapClean = do
        let cln (AppOp1 OpSwap e) = pure e
            cln _ = Nothing
        return (PTuple p2 p1, cln)
patClean _ = []
