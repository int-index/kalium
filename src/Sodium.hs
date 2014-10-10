{-# LANGUAGE FlexibleContexts #-}
module Sodium (translate) where

import Sodium.Nucleus.Vectorize   (vectorize)
import Sodium.Nucleus.IOMagic     (uncurse)
import Sodium.Nucleus.Side        (side)
import Sodium.Nucleus.Pass.Flatten     (flatten)
import Sodium.Nucleus.Pass.JoinMultiIf (joinMultiIf)
import Sodium.Nucleus.Pass.Inline      (inline)
import Sodium.Nucleus.Pass.FoldMatch   (foldMatch)
import Sodium.Nucleus.Pass.ExtractBody (extractBody)
import Sodium.Nucleus.Pass.BindClean   (bindClean)
import Sodium.Nucleus.Pass.Clean       (clean)
import Sodium.Nucleus.Pass.Compute     (compute)
import Sodium.Pascal.Parse   (parse)
import Language.Haskell.Exts.Pretty (prettyPrint)
import qualified  Sodium.Pascal.Convert as P (convert)
import qualified Sodium.Haskell.Convert as H (convert)
import qualified Sodium.Error as E
import qualified Sodium.Nucleus.Program.Vector as V
import qualified Sodium.Nucleus.Render as R

import Data.Bool
import Control.Monad.Writer hiding (pass)
import Control.Monad.State
import Control.Monad.Except

import Debug.Trace

translate :: MonadError E.Error m => String -> m String
translate src = do
    pas <- liftErr E.parseError (parse src)
    let scalar = (side <=< (return . uncurse) <=< P.convert) pas
          `evalState` map (V.Name ["g"] . show) [0..]
    vector <- liftErr E.vectorizeError (vectorize scalar)
    let optimal = let (a, log) = runWriter (closureM pass vector)
                  in trace (concat $ map R.render log) a
    return $ prettyPrint (H.convert optimal)

liftErr :: MonadError e' m => (e -> e') -> Except e a -> m a
liftErr h m = either (throwError . h) return (runExcept m)

pass :: MonadWriter [V.Program] m => V.Program -> m V.Program
pass program = tell [program] >> f program
    where f = return
            . compute   . flatten     . inline
            . foldMatch . joinMultiIf . extractBody
            . bindClean . clean

closureM :: (Eq a, Monad m) => (a -> m a) -> (a -> m a)
closureM f = go where
    go x = f x >>= \y -> bool (go y) (return x) (x == y)
