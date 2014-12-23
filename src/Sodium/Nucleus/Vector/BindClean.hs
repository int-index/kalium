module Sodium.Nucleus.Vector.BindClean where

import Control.Applicative
import Control.Lens
import Data.Foldable
import Sodium.Nucleus.Vector.Program
import Sodium.Nucleus.Vector.Recmap
import Sodium.Nucleus.Vector.Attempt
import Sodium.Util

bindClean :: Program -> Program
bindClean = over recmapped bindCleanExpression

bindCleanExpression :: Expression -> Expression
bindCleanExpression = \case
    Follow p x a | Just e <- asum $ map (taintSubst x a) (patClean p) -> e
    Into   p x a | Just e <- asum $ map (pureSubst  x a) (patClean p) -> e
    e -> e

taintSubst :: Expression -> Expression -> (Pattern, Attempt) -> Maybe Expression
taintSubst x a (p, c) = taintAttempt c x <&> \x' -> Follow p x' a

pureSubst :: Expression -> Expression -> (Pattern, Attempt) -> Maybe Expression
pureSubst x a (p, c) = propagate c x <&> \x' -> Into p x' a

type Con = forall a . (a,a) -> (a,a)

patClean :: Pattern -> Pairs Pattern Attempt
patClean p@PWildCard = return (p, \_ -> return LitUnit)
patClean (PTuple PWildCard p) = return (p, Just . AppOp1 OpSnd)
patClean (PTuple p PWildCard) = return (p, Just . AppOp1 OpFst)
patClean (PTuple p1 p2) = fstClean ++ sndClean where
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
patClean _ = []
