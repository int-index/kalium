{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
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
instance MetaSource MetaName       where metaFromInt =  MetaName
instance MetaSource MetaPName      where metaFromInt =  MetaPName
instance MetaSource MetaExpression where metaFromInt =  Ext . metaFromInt
instance MetaSource MetaPattern    where metaFromInt = PExt . metaFromInt

metaSource :: MetaSource a => [a]
metaSource = metaFromInt <$> [0..]

metaSource2 :: (MetaSource a, MetaSource b) => ([a], [b])
metaSource2 = (metaSource, metaSource)

type MetaMonad = ReaderT MetaTable Maybe

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


metaSubst :: MetaExpression -> MetaMonad Expression
metaSubst = \case
    Primary l -> pure (Primary l)
    Access  n -> pure (Access  n)
    Lambda p a -> Lambda <$> metaPSubst p <*> metaSubst a
    Beta f a -> Beta <$> metaSubst f <*> metaSubst a
    Ext metaname -> viewMetaExpression metaname


metaPSubst :: MetaPattern -> MetaMonad Pattern
metaPSubst = \case
    PWildCard -> pure PWildCard
    PUnit -> pure PUnit
    PAccess n t -> pure (PAccess n t)
    PTuple p1 p2 -> PTuple <$> metaPSubst p1 <*> metaPSubst p2
    PExt metapname -> viewMetaPattern metapname


type MetaModifier = MetaTable -> Maybe MetaTable

data Rule
    = MetaExpression := MetaExpression
    | Rule :> MetaModifier

infixl 7 :=
infixl 7 :>

ruleMatch :: Rule -> Expression -> Maybe Expression
ruleMatch = ruleMatch' return

ruleMatch' :: MetaModifier -> Rule -> Expression -> Maybe Expression
ruleMatch' metamod (rule :> metamod') = ruleMatch' (metamod >=> metamod') rule
ruleMatch' metamod (lhs := rhs)
      = \expr
     -> metaMatch lhs expr
    >>= metamod
    >>= runReaderT (metaSubst rhs)

viewMetaExpression :: MetaName -> MetaMonad Expression
viewMetaExpression metaname = views metaExpressionTable (M.lookup metaname) >>= lift

viewMetaPattern :: MetaPName -> MetaMonad Pattern
viewMetaPattern metapname = views metaPatternTable (M.lookup metapname) >>= lift

constraint :: MetaMonad Bool -> MetaModifier
constraint ma = runReaderT $ ma >>= guard >> ask

constraint1 c x1    = constraint (c <$> extract x1)
constraint2 c x1 x2 = constraint (c <$> extract x1 <*> extract x2)

type family Extracted a where
    Extracted MetaExpression = Expression
    Extracted MetaPattern = Pattern

class Extract a where
    extract :: a -> MetaMonad (Extracted a)

instance Extract MetaExpression where
    extract = \case { Ext metaname -> viewMetaExpression metaname; _ -> empty }

instance Extract MetaPattern where
    extract = \case { PExt metapname -> viewMetaPattern metapname; _ -> empty }

fire :: [Rule] -> Endo' Expression
fire = foldr (.) id . map (tryApply . ruleMatch)
