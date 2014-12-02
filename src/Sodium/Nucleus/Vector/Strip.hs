{-# LANGUAGE ViewPatterns #-}
module Sodium.Nucleus.Vector.Strip (strip) where

import Data.List
import Data.Ord
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.Writer

import Sodium.Nucleus.Vector.Program
import Sodium.Nucleus.Vector.Name

-- TODO: apply multiple times for nested namespaces
strip :: Mask a => [String] -> a -> a
strip reserved a = runReader (mask a) (return . r)
    where r = appEndo (resolve (map (retag . Name . return) reserved) (collect a))

collect :: Mask a => a -> S.Set (Name1 IndexTag)
collect a = execWriter (runReaderT (mask a) check)
    where check name = tell (S.singleton name) >> return name

freq :: S.Set (Name1 IndexTag) -> M.Map (Name1 IndexTag) (M.Map NameSpace Int)
freq = S.foldr go M.empty
    where go (nsSplit -> (name, ns))
            = M.insertWith (M.unionWith (+)) name (M.singleton ns 1)

mostfreq :: M.Map NameSpace Int -> NameSpace
mostfreq = fst . maximumBy (comparing snd) . M.toList

resolve :: [Name1 IndexTag] -> S.Set (Name1 IndexTag) -> Endo (Name1 IndexTag)
resolve reserved collected = M.foldMapWithKey go (freq collected)
    where go name (mostfreq -> ns) = Endo $ \case
            (nsSplit -> (name', ns'))
                | name' == name, ns' == ns, name `notElem` reserved
                -> name
            x -> x

nsSplit :: Name1 IndexTag -> (Name1 IndexTag, NameSpace)
nsSplit (Name1 (n:ns) tag) = (Name1 ns tag, Just n)
nsSplit name = (name, Nothing)

type NameSpace = Maybe String
