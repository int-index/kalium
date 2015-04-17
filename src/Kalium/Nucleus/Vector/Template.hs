{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Kalium.Nucleus.Vector.Template where

import Data.String
import qualified Data.Map as M

import Kalium.Prelude
import Kalium.Util
import Kalium.Nucleus.Vector.Program

data family MetaReference a

newtype instance MetaReference Expression = MetaName String
    deriving (Eq, Ord)

newtype instance MetaReference Pattern = MetaPName String
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

makeLenses ''MetaTable

newtype MetaTableBuilder = MetaTableBuilder { getMetaTableBuilder :: Maybe MetaTable }

runMetaTableBuilder :: (a -> MetaTableBuilder) -> (a -> Maybe MetaTable)
runMetaTableBuilder = (getMetaTableBuilder .)

instance Monoid MetaTableBuilder where
    mempty = MetaTableBuilder (Just (MetaTable M.empty M.empty))
    mappend (MetaTableBuilder m1) (MetaTableBuilder m2) = MetaTableBuilder $ do
        MetaTable me1 mp1 <- m1
        MetaTable me2 mp2 <- m2
        MetaTable <$> unionWithSame me1 me2 <*> unionWithSame mp1 mp2

class MetaTableBuilderYield a where
    metaYield :: MetaReference a -> a -> MetaTableBuilder

instance MetaTableBuilderYield Expression where
    metaYield metaname expr
        = MetaTableBuilder . Just
        $ MetaTable (M.singleton metaname expr) mempty

instance MetaTableBuilderYield Pattern where
    metaYield metapname pat
        = MetaTableBuilder . Just
        $ MetaTable mempty (M.singleton metapname pat)

-- TODO: MonoidZero or something

guardMetaTableBuilder :: Bool -> MetaTableBuilder
guardMetaTableBuilder = bool zeroMetaTableBuilder mempty

zeroMetaTableBuilder :: MetaTableBuilder
zeroMetaTableBuilder = MetaTableBuilder Nothing

instance IsString MetaName  where fromString = MetaName
instance IsString MetaPName where fromString = MetaPName
instance IsString
  ( Expression' (MetaReference Pattern) (MetaReference Expression)
  ) where fromString =  Ext . fromString
instance IsString
  ( Pattern' (MetaReference Pattern)
  ) where fromString = PExt . fromString

-- begin MetaMonad

newtype MetaTableReader a = MetaTableReader (ReaderT MetaTable Maybe a)
    deriving (Functor, Applicative, Alternative, Monad, MonadFix, MonadPlus)

runMetaTableReader :: MetaTableReader a -> MetaTable -> Maybe a
runMetaTableReader (MetaTableReader t) = runReaderT t

newtype MetaTableState a = MetaTableState (StateT MetaTable Maybe a)
    deriving (Functor, Applicative, Alternative, Monad, MonadFix, MonadPlus)

class (Alternative m, MonadPlus m) => GetMetaTable m where
    getMetaTable :: m MetaTable

instance GetMetaTable MetaTableReader where
    getMetaTable = MetaTableReader ask

instance GetMetaTable MetaTableState where
    getMetaTable = MetaTableState get

class ToMetaModifier m where
    type family ToMetaModifierReturn m :: *
    toMetaModifier :: m (ToMetaModifierReturn m) -> MetaModifier

instance ToMetaModifier MetaTableReader where
    type ToMetaModifierReturn MetaTableReader = MetaTable
    toMetaModifier = runMetaTableReader

instance ToMetaModifier MetaTableState where
    type ToMetaModifierReturn MetaTableState = ()
    toMetaModifier (MetaTableState t) = execStateT t

-- end MetaMonad

metaMatch :: Meta Expression -> Expression -> MetaTableBuilder

metaMatch (Ext metaname) expr = metaYield metaname expr

metaMatch (Primary l') (Primary l) = guardMetaTableBuilder (l' == l)
metaMatch (Access  n') (Access  n) = guardMetaTableBuilder (n' == n)

metaMatch (Beta   f' a') (Beta   f a) = metaMatch  f' f <> metaMatch a' a
metaMatch (Lambda p' a') (Lambda p a) = metaPMatch p' p <> metaMatch a' a

metaMatch _ (Ext ext) = absurd ext
metaMatch _ _ = zeroMetaTableBuilder


metaPMatch :: Meta Pattern -> Pattern -> MetaTableBuilder

metaPMatch (PExt metapname) pat = metaYield metapname pat

metaPMatch PWildCard PWildCard = mempty
metaPMatch PUnit     PUnit     = mempty
metaPMatch (PAccess n' t') (PAccess n t)
     = guardMetaTableBuilder (n' == n)
    <> guardMetaTableBuilder (t' == t)

metaPMatch (PTuple p1' p2') (PTuple p1 p2)
    = metaPMatch p1' p1 <> metaPMatch p2' p2

metaPMatch _ (PExt pext) = absurd pext
metaPMatch _ _ = zeroMetaTableBuilder


metaSubst :: (GetMetaTable m, Alternative m) => Meta Expression -> m Expression
metaSubst = \case
    Primary l -> pure (Primary l)
    Access  n -> pure (Access  n)
    Lambda p a -> Lambda <$> metaPSubst p <*> metaSubst a
    Beta f a -> Beta <$> metaSubst f <*> metaSubst a
    Ext metaname -> viewMetaObject metaname


metaPSubst :: (GetMetaTable m, Alternative m) => Meta Pattern -> m Pattern
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
infixl 7 .:>

rule .:> metamod = rule :> toMetaModifier metamod

ruleMatch :: Rule -> Expression -> Maybe Expression
ruleMatch = ruleMatch' return

ruleMatch' :: MetaModifier -> Rule -> Expression -> Maybe Expression
ruleMatch' metamod (rule :> metamod') = ruleMatch' (metamod >=> metamod') rule
ruleMatch' metamod (lhs := rhs)
     =  runMetaTableBuilder (metaMatch lhs)
    >=> metamod
    >=> runMetaTableReader (metaSubst rhs)

class Ord (MetaReference a) => MetaObjectSubtable a where
    metaObjectSubtable :: Lens' MetaTable (MetaSubtable a)

instance MetaObjectSubtable Expression where
    metaObjectSubtable = metaExpressionTable

instance MetaObjectSubtable Pattern where
    metaObjectSubtable = metaPatternTable

viewMetaObject
    :: (MetaObjectSubtable a, GetMetaTable m, Alternative m)
    => MetaReference a -> m a
viewMetaObject metaname = do
    mobj <- views metaObjectSubtable (M.lookup metaname) <$> getMetaTable
    amaybe mobj

class GetMetaReference a where
    getMetaReference :: Alternative m => Meta a -> m (MetaReference a)

instance GetMetaReference Expression where
    getMetaReference = \case { Ext metaname -> pure metaname; _ -> empty }

instance GetMetaReference Pattern where
    getMetaReference = \case { PExt metapname -> pure metapname; _ -> empty }


constraint :: MetaTableReader Bool -> MetaModifier
constraint ma = runMetaTableReader $ ma >>= guard >> getMetaTable

constraint1 c x1    = constraint (c <$> mmeta x1)
constraint2 c x1 x2 = constraint (c <$> mmeta x1 <*> mmeta x2)

type MetaObject a = (MetaObjectSubtable a, GetMetaReference a)

transform :: MetaObject a => (a -> Maybe a) -> Meta a -> MetaModifier
transform fn metaobj = (metaobj --> metaobj) fn

settingMetaReference
    :: (MetaObjectSubtable a, GetMetaTable m)
    => a -> MetaReference a -> m MetaTable
settingMetaReference a metaname = over metaObjectSubtable (M.insert metaname a) <$> getMetaTable

settingMetaObject
    :: (MetaObject a, GetMetaTable m, Alternative m)
    => Meta a -> a -> m MetaTable
settingMetaObject metaobj a = getMetaReference metaobj >>= settingMetaReference a

(..=) :: MetaObject a => Meta a -> a -> MetaTableState ()
metaobj ..= a = settingMetaObject metaobj a >>= MetaTableState . put

(-->)
    :: (MetaObject a, MetaObject b)
    => Meta a -> Meta b -> (a -> Maybe b) -> MetaModifier
(-->) metaobj metaobj' fn = toMetaModifier $ do
    a <- mmeta metaobj
    b <- amaybe (fn a)
    metaobj' ..= b

mmeta :: (MetaObject a, GetMetaTable m, Alternative m) => Meta a -> m a
mmeta = getMetaReference >=> viewMetaObject

fire :: [Rule] -> Endo' Expression
fire = foldr (.) id . map (tryApply . ruleMatch)
