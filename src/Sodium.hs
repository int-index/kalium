module Sodium (translate) where

import Sodium.Nucleus.Vectorize   (vectorize)
import Sodium.Nucleus.IOMagic     (uncurse)
import Sodium.Nucleus.Side        (side)
import Sodium.Nucleus.Pass.Flatten     (flatten)
import Sodium.Nucleus.Pass.JoinMultiIf (joinMultiIf)
import Sodium.Nucleus.Pass.Inline      (inline)
import Sodium.Nucleus.Pass.FoldMatch   (foldMatch)
import Sodium.Nucleus.Pass.ExtractBody (extractBody)
import Sodium.Nucleus.Pass.Clean       (clean)
import Sodium.Pascal.Parse   (parse)
import Language.Haskell.Exts.Pretty (prettyPrint)
import qualified  Sodium.Pascal.Convert as P (convert)
import qualified Sodium.Haskell.Convert as H (convert)
import qualified Sodium.Error as E

import Control.Monad.Except
import Data.Profunctor

import Debug.Trace

translate :: String -> String
translate = dimap fromPascal toHaskell onNucleus where
    fromPascal = P.convert . parse'
    toHaskell  = prettyPrint . H.convert
    onNucleus = dimap onScalar onVector vectorize'
    onScalar = side . uncurse
    onVector = closure pass
    pass = flatten . inline . foldMatch . joinMultiIf . extractBody . clean

parse'     = error' . withExcept E.parseError     . parse
vectorize' = error' . withExcept E.vectorizeError . vectorize

error' = either (error.show) id . runExcept

closure :: Eq a => (a -> a) -> a -> a
closure f = match 0 . iterate f
  where match n (a:b:_) | a == b = trace ("Pass amount: " ++ show n) a
        match n (_:c) = match (succ n) c
