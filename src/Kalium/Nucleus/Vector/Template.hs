{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Kalium.Nucleus.Vector.Template where

import qualified Data.Map as M

import Kalium.Prelude
import Kalium.Util
import Kalium.Nucleus.Vector.Program

newtype MetaName = MetaName Int
    deriving (Eq, Ord)

newtype MetaPName = MetaPName Int
    deriving (Eq, Ord)

type MetaPattern = Pattern' MetaPName
type MetaExpression = Expression' MetaPName MetaName

data MetaTable = MetaTable
    { _metaExpressionTable :: Map MetaName  Expression
    , _metaPatternTable    :: Map MetaPName Pattern
    }

singletonMetaExpressionTable :: MetaName -> Expression -> MetaTable
singletonMetaExpressionTable metaname expr
    = MetaTable (M.singleton metaname expr) M.empty

singletonMetaPatternTable :: MetaPName -> Pattern -> MetaTable
singletonMetaPatternTable metapname pat
    = MetaTable M.empty (M.singleton metapname pat)

done = Just (MetaTable M.empty M.empty)
m1 <+> m2 = do
    MetaTable me1 mp1 <- m1
    MetaTable me2 mp2 <- m2
    MetaTable <$> unionWithSame me1 me2 <*> unionWithSame mp1 mp2

makeLenses ''MetaTable

class    MetaSource     a          where metaFromInt :: Int -> a
instance MetaSource MetaExpression where metaFromInt =  Ext . MetaName
instance MetaSource MetaPattern    where metaFromInt = PExt . MetaPName

metaSource :: MetaSource a => [a]
metaSource = metaFromInt <$> [0..]

metaSource2 :: (MetaSource a, MetaSource b) => ([a], [b])
metaSource2 = (metaSource, metaSource)

metaMatch :: MetaExpression -> Expression -> Maybe MetaTable

metaMatch (Ext metaname) expr
    = pure (singletonMetaExpressionTable metaname expr)

metaMatch (Primary l') (Primary l) = guard (l' == l) >> done
metaMatch (Access  n') (Access  n) = guard (n' == n) >> done

metaMatch (Beta   f' a') (Beta   f a) = metaMatch  f' f <+> metaMatch a' a
metaMatch (Lambda p' a') (Lambda p a) = metaPMatch p' p <+> metaMatch a' a

metaMatch _ (Ext ext) = absurd ext
metaMatch _ _ = empty


metaPMatch :: MetaPattern -> Pattern -> Maybe MetaTable

metaPMatch (PExt metapname) pat
    = pure (singletonMetaPatternTable metapname pat)

metaPMatch PWildCard PWildCard = done
metaPMatch PUnit     PUnit     = done
metaPMatch (PAccess n' t') (PAccess n t) = do
    guard (n' == n)
    guard (t' == t)
    done

metaPMatch (PTuple p1' p2') (PTuple p1 p2)
    = metaPMatch p1' p1 <+> metaPMatch p2' p2

metaPMatch _ (PExt pext) = absurd pext
metaPMatch _ _ = empty


metaSubst :: MetaExpression -> ReaderT MetaTable Maybe Expression
metaSubst = \case
    Primary l -> pure (Primary l)
    Access  n -> pure (Access  n)
    Lambda p a -> Lambda <$> metaPSubst p <*> metaSubst a
    Beta f a -> Beta <$> metaSubst f <*> metaSubst a
    Ext metaname -> views metaExpressionTable (M.lookup metaname) >>= lift


metaPSubst :: MetaPattern -> ReaderT MetaTable Maybe Pattern
metaPSubst = \case
    PWildCard -> pure PWildCard
    PUnit -> pure PUnit
    PAccess n t -> pure (PAccess n t)
    PTuple p1 p2 -> PTuple <$> metaPSubst p1 <*> metaPSubst p2
    PExt metapname -> views metaPatternTable (M.lookup metapname) >>= lift


data Rule = MetaExpression := MetaExpression

ruleMatch :: Rule -> Expression -> Maybe Expression
ruleMatch (lhs := rhs) expr = metaMatch lhs expr >>= runReaderT (metaSubst rhs)

fire :: [Rule] -> Endo' Expression
fire = foldr (.) id . map (tryApply . ruleMatch)
