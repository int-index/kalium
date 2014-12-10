module Sodium.Nucleus.Vector.BindClean where

import Control.Applicative
import Control.Monad
import Control.Lens
import Data.Foldable
import Data.Function
import Data.Tuple
import Sodium.Nucleus.Vector.Program
import Sodium.Nucleus.Vector.Recmap
import Sodium.Util

bindClean :: Program -> Program
bindClean = over recmapped bindCleanExpression

type Cleaner = Expression -> Maybe Expression

bindCleanExpression :: Expression -> Expression
bindCleanExpression = \case
    Follow p x a | Just e <- asum $ map (subst x a) (patClean p) -> e
    e -> e

subst :: Expression -> Expression -> (Pattern, Cleaner) -> Maybe Expression
subst x a (p, c) = cleanRet c x <&> \x' -> Follow p x' a

cleanRet :: Cleaner -> Cleaner
cleanRet c = fix $ \go -> \case
    Taint a -> Taint <$> c a
    Follow p x a -> Follow p x <$> go a
    AppOp3 OpIf xElse xThen cond
        -> AppOp3 OpIf <$> go xElse <*> go xThen <*> pure cond
    _ -> Nothing

type Con = forall a . (a,a) -> (a,a)

patClean :: Pattern -> Pairs Pattern Cleaner
patClean p@PWildCard = return (p, \_ -> return LitUnit)
patClean (PTuple pat1 pat2) = go id OpFst `mplus` go swap OpSnd where
    go :: Con -> Operator -> Pairs Pattern Cleaner
    go con op = patClean p2 >>= wrap where
        (p1, p2) = con (pat1, pat2)
        wrap (PWildCard, _) = do
            let cln expr = return (AppOp1 op expr)
            return (p1, cln)
        wrap (p0, cln') = do
            let cln (AppOp2 OpPair expr1 expr2) = do
                  let (act1, act2) = con (pure, cln')
                  AppOp2 OpPair <$> act1 expr1 <*> act2 expr2
                cln _ = Nothing
            return (PTuple `uncurry` con (p1, p0), cln)

patClean _ = []
