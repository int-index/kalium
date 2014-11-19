{-# LANGUAGE ViewPatterns #-}
module Sodium.Nucleus.Strip (strip) where

import Data.List
import Data.Ord
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.Writer

import Sodium.Nucleus.Program
import Sodium.Nucleus.Name

-- TODO: apply multiple times for nested namespaces
strip :: Mask a => [String] -> a -> a
strip reserved a = runReader (mask a) (return . r)
    where r = appEndo (resolve (map (Name . return) reserved) (collect a))

collect :: Mask a => a -> S.Set Name
collect a = execWriter (runReaderT (mask a) check)
    where check name = tell (S.singleton name) >> return name

freq :: S.Set Name -> M.Map Name (M.Map NameSpace Int)
freq = S.foldr go M.empty
    where go (nsSplit -> (name, ns))
            = M.insertWith (M.unionWith (+)) name (M.singleton ns 1)

mostfreq :: M.Map NameSpace Int -> NameSpace
mostfreq = fst . maximumBy (comparing snd) . M.toList

resolve :: [Name] -> S.Set Name -> Endo Name
resolve reserved collected = M.foldMapWithKey go (freq collected)
    where go name (mostfreq -> ns) = Endo $ \case
            (nsSplit -> (name', ns'))
                | name' == name, ns' == ns, name `notElem` reserved
                -> name
            x -> x

nsSplit :: Name -> (Name, NameSpace)
nsSplit (Name (n:ns)) = (Name ns, Just n)
nsSplit name = (name, Nothing)

type NameSpace = Maybe String
