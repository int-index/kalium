{-# LANGUAGE FlexibleContexts #-}
module Sodium (translate) where

import Sodium.Nucleus.Vectorize   (vectorize)
import Sodium.Nucleus.Scalar.Atomize (atomize')
import Sodium.Nucleus.Scalar.Valueficate (valueficate)
import Sodium.Nucleus.Vector.Strip (strip)
import Sodium.Nucleus.Vector.Match (match)
import Sodium.Nucleus.Vector.Inline (inline)
import Language.Haskell.Exts.Pretty (prettyPrint)
import qualified Sodium.Pascal.Parse   as P (parse)
import qualified Sodium.Pascal.Convert as P (convert)
import qualified Sodium.Haskell.Convert as H (convert, reserved)
import qualified Sodium.Error as E
import qualified Sodium.Nucleus.Vector.Program as V

import Data.Bool
import Control.Applicative
import Control.Monad.Writer hiding (pass)
import Control.Monad.Except
import Control.Monad.Supply

namestack :: [V.Name1 ()]
namestack = map ((\name -> V.Name1 ["g", name] ()) . ('g':) . show) [0..]

translate :: (Applicative m, MonadError E.Error m) => String -> m ([String], String)
translate src = do
    pas <- P.parse src
    (optimal, log) <- flip evalSupplyT namestack
          $ P.convert pas
        >>= atomize'
        >>= atomize' . valueficate
        >>= vectorize
        >>= optimize
    let dest = prettyPrint $ H.convert $ strip H.reserved optimal
    return (map (prettyPrint . H.convert) log, dest)

type TranslationLog = [V.Program]

optimize :: (Applicative m, MonadSupply (V.Name1 ()) m) => V.Program -> m (V.Program, TranslationLog)
optimize program = runWriterT (closureM pass program)

pass :: (Applicative m, MonadWriter TranslationLog m, MonadSupply (V.Name1 ()) m) => V.Program -> m V.Program
pass program = tell [program] >> f program
    where f  =  return . match . inline

closureM :: (Eq a, Monad m) => (a -> m a) -> (a -> m a)
closureM f = go where
    go x = f x >>= \y -> bool (go y) (return x) (x == y)
