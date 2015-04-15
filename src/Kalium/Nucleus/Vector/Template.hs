{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

class (Alternative m, MonadPlus m) => SetMetaTable m where
    setMetaTable :: MetaTable -> m ()

instance SetMetaTable MetaTableState where
    setMetaTable = MetaTableState . put

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

ruleMatch :: Rule -> Expression -> Maybe Expression
ruleMatch = ruleMatch' return

ruleMatch' :: MetaModifier -> Rule -> Expression -> Maybe Expression
ruleMatch' metamod (rule :> metamod') = ruleMatch' (metamod >=> metamod') rule
ruleMatch' metamod (lhs := rhs)
      = \expr
     -> metaMatch lhs expr
    >>= metamod
    >>= runMetaTableReader (metaSubst rhs)

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
    maybe empty pure mobj

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

(...=) :: MetaObject a => Meta a -> Maybe a -> MetaTableState ()
metaobj ...= ma = do
    a <- maybe empty pure ma
    metaobj ..= a

(..=) :: MetaObject a => Meta a -> a -> MetaTableState ()
metaobj ..= a = settingMetaObject metaobj a >>= setMetaTable

metaExecWith1 a     fn = toMetaModifier            (mmeta a >>= fn)
metaExecWith2 a b   fn = metaExecWith1 a     (\a -> mmeta b >>= fn a)
metaExecWith3 a b c fn = metaExecWith2 a b (\a b -> mmeta c >>= fn a b)

(-->)
    :: (MetaObject a, MetaObject b)
    => Meta a -> Meta b
    -> (a -> Maybe b) -> MetaModifier
(-->) metaobj metaobj' fn = toMetaModifier $ do
    a <- mmeta metaobj
    metaobj' ...= fn a

mmeta :: (MetaObject a, GetMetaTable m, Alternative m) => Meta a -> m a
mmeta = getMetaReference >=> viewMetaObject

fire :: [Rule] -> Endo' Expression
fire = foldr (.) id . map (tryApply . ruleMatch)
