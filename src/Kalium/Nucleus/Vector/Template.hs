{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Kalium.Nucleus.Vector.Template where

import qualified Data.Map as M

import Kalium.Prelude
import Kalium.Util
import Kalium.Nucleus.Vector.Program

data family MetaReference a

newtype instance MetaReference Expression = MetaName Int
    deriving (Eq, Ord)

newtype instance MetaReference Pattern = MetaPName Int
    deriving (Eq, Ord)

type family Meta a where
    Meta Expression = Expression' (MetaReference Pattern) (MetaReference Expression)
    Meta Pattern = Pattern' (MetaReference Pattern)

type MetaName  = MetaReference Expression
type MetaPName = MetaReference Pattern

type MetaSubtable a = Map (MetaReference a) a

data MetaTable = MetaTable
    { _metaExpressionTable :: MetaSubtable Expression
    , _metaPatternTable    :: MetaSubtable Pattern
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

class    MetaSource     a     where metaFromInt :: Int -> a
instance MetaSource MetaName  where metaFromInt =  MetaName
instance MetaSource MetaPName where metaFromInt =  MetaPName

instance MetaSource
  ( Expression' (MetaReference Pattern) (MetaReference Expression)
  ) where metaFromInt =  Ext . metaFromInt

instance MetaSource
  ( Pattern' (MetaReference Pattern)
  ) where metaFromInt = PExt . metaFromInt

metaSource :: MetaSource a => [a]
metaSource = metaFromInt <$> [0..]

metaSource2 :: (MetaSource a, MetaSource b) => ([a], [b])
metaSource2 = (metaSource, metaSource)

type MetaMonad = ReaderT MetaTable Maybe

metaMatch :: Meta Expression -> Expression -> Maybe MetaTable

metaMatch (Ext metaname) expr
    = pure (singletonMetaExpressionTable metaname expr)

metaMatch (Primary l') (Primary l) = guard (l' == l) >> done
metaMatch (Access  n') (Access  n) = guard (n' == n) >> done

metaMatch (Beta   f' a') (Beta   f a) = metaMatch  f' f <+> metaMatch a' a
metaMatch (Lambda p' a') (Lambda p a) = metaPMatch p' p <+> metaMatch a' a

metaMatch _ (Ext ext) = absurd ext
metaMatch _ _ = empty


metaPMatch :: Meta Pattern -> Pattern -> Maybe MetaTable

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


metaSubst :: Meta Expression -> MetaMonad Expression
metaSubst = \case
    Primary l -> pure (Primary l)
    Access  n -> pure (Access  n)
    Lambda p a -> Lambda <$> metaPSubst p <*> metaSubst a
    Beta f a -> Beta <$> metaSubst f <*> metaSubst a
    Ext metaname -> viewMetaObject metaname


metaPSubst :: Meta Pattern -> MetaMonad Pattern
metaPSubst = \case
    PWildCard -> pure PWildCard
    PUnit -> pure PUnit
    PAccess n t -> pure (PAccess n t)
    PTuple p1 p2 -> PTuple <$> metaPSubst p1 <*> metaPSubst p2
    PExt metapname -> viewMetaObject metapname


type MetaModifier = MetaTable -> Maybe MetaTable

data Rule
    = Meta Expression := Meta Expression
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

class Ord (MetaReference a) => MetaObjectSubtable a where
    metaObjectSubtable :: Lens' MetaTable (MetaSubtable a)

instance MetaObjectSubtable Expression where
    metaObjectSubtable = metaExpressionTable

instance MetaObjectSubtable Pattern where
    metaObjectSubtable = metaPatternTable

viewMetaObject :: MetaObjectSubtable a => MetaReference a -> MetaMonad a
viewMetaObject metaname = views metaObjectSubtable (M.lookup metaname) >>= lift

class GetMetaReference a where
    getMetaReference :: Meta a -> MetaMonad (MetaReference a)

instance GetMetaReference Expression where
    getMetaReference = \case { Ext metaname -> return metaname; _ -> empty }

instance GetMetaReference Pattern where
    getMetaReference = \case { PExt metapname -> return metapname; _ -> empty }


constraint :: MetaMonad Bool -> MetaModifier
constraint ma = runReaderT $ ma >>= guard >> ask

constraint1 c x1    = constraint (c <$> mmeta x1)
constraint2 c x1 x2 = constraint (c <$> mmeta x1 <*> mmeta x2)

type MetaObject a = (MetaObjectSubtable a, GetMetaReference a)

transform :: MetaObject a => (a -> Maybe a) -> Meta a -> MetaModifier
transform fn metaobj = runReaderT $ do
    metaname <- getMetaReference metaobj
    expr <- viewMetaObject metaname
    case fn expr of
        Nothing -> empty
        Just a -> over metaObjectSubtable (M.insert metaname a) <$> ask


mmeta :: MetaObject a => Meta a -> MetaMonad a
mmeta = getMetaReference >=> viewMetaObject

fire :: [Rule] -> Endo' Expression
fire = foldr (.) id . map (tryApply . ruleMatch)
