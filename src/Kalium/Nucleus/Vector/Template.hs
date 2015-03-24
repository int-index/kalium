module Kalium.Nucleus.Vector.Template where

import qualified Data.Map as M

import Kalium.Prelude
import Kalium.Util
import Kalium.Nucleus.Vector.Program
import Kalium.Nucleus.Vector.Pattern
import Kalium.Nucleus.Vector.Name

newtype MetaName = MetaName Int
    deriving (Eq, Ord)

data MetaExt expr = MetaExt MetaName

type MetaExpression = Expression' MetaExt

pattern Meta n = Ext (MetaExt n)

type MetaTable = Map MetaName Expression

metaSource :: [MetaExpression]
metaSource = Meta . MetaName <$> [0..]

metaMatch :: MetaExpression -> Expression -> Maybe MetaTable

metaMatch (Meta metaname) expr = pure (M.singleton metaname expr)

metaMatch (Primary l') (Primary l) | l' == l = pure mempty
metaMatch (Access  n') (Access  n) | n' == n = pure mempty

metaMatch (Beta f' a') (Beta f  a) = do
    m1 <- metaMatch f' f
    m2 <- metaMatch a' a
    unionWithSame m1 m2

metaMatch (Lambda p' a') (Lambda p a)
    | Just alpha <- alphaMatch p p'
    = metaMatch a' (alphaRename alpha a)

metaMatch _ (Ext ext) = absurd (getConst ext)
metaMatch _ _ = empty


metaSubst :: MetaExpression -> ReaderT MetaTable Maybe Expression
metaSubst = \case
    Primary l -> pure (Primary l)
    Access  n -> pure (Access  n)
    Lambda p a -> Lambda p <$> metaSubst a
    Beta f a -> Beta <$> metaSubst f <*> metaSubst a
    Meta metaname -> asks (M.lookup metaname) >>= lift

    _ -> error "impossible"
    -- ^ this one is to silence GHC warning about non-exhaustive
    --   pattern matching as it fails to see the exhaustiveness
    --   due to pattern synonyms


data Rule = MetaExpression := MetaExpression

ruleMatch :: Rule -> Expression -> Maybe Expression
ruleMatch (lhs := rhs) expr = metaMatch lhs expr >>= runReaderT (metaSubst rhs)

fire :: [Rule] -> Endo' Expression
fire = foldr (.) id . map (tryApply . ruleMatch)
