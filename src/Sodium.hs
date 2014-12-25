{-# LANGUAGE FlexibleContexts #-}
module Sodium (translate) where

import Sodium.Prelude

import Sodium.Nucleus.Vectorize (vectorize)
import Sodium.Nucleus.Scalar.Atomize (atomize')
import Sodium.Nucleus.Scalar.Valueficate (valueficate)
import Sodium.Nucleus.Vector.Match (match)
import Sodium.Nucleus.Vector.Inline (inline, reorder)
import Sodium.Nucleus.Vector.ArgClean (argClean)
import Sodium.Nucleus.Vector.BindClean (bindClean)
import Sodium.Nucleus.Vector.Context (extractCtx)
import Sodium.Nucleus.Vector.Sanity (sanity_nameUniqueness)
import Language.Haskell.Exts.Pretty (prettyPrint)
import qualified Sodium.Pascal.Parse   as P (parse)
import qualified Sodium.Pascal.Convert as P (convert)
import qualified Sodium.Haskell.Sugar   as H (sugarcoat)
import qualified Sodium.Haskell.Convert as H (convert)
import qualified Sodium.Error as E
import qualified Sodium.Nucleus.Vector.Program as V
import Sodium.Util (closureM)

namestack :: [Integer]
namestack = [0..]

sanity_check name f x
    | f x = return x
    | otherwise = throwError (E.Insane name)

translate :: (Applicative m, MonadError E.Error m) => String -> m ([String], String)
translate src = do
    pas <- P.parse src
    (optimal, log) <- flip evalSupplyT namestack
          $ P.convert pas
        >>= atomize'
        >>= atomize' . valueficate
        >>= vectorize
        >>= sanity_check "Name uniqueness" sanity_nameUniqueness
        >>= optimize
    let sweet = H.sugarcoat (H.convert optimal)
    return (map (prettyPrint . H.convert) log, prettyPrint sweet)

type TranslationLog = [V.Program]

optimize :: (Applicative m, Monad m, MonadSupply Integer m) => V.Program -> m (V.Program, TranslationLog)
optimize program = runWriterT (closureM optimizeStep program)

logging :: MonadWriter [a] m => (a -> m a) -> (a -> m a)
logging f x = tell [x] >> f x

optimizeStep :: (Applicative m, MonadWriter TranslationLog m, MonadSupply Integer m) => V.Program -> m V.Program
optimizeStep = closureM (logging f) >=> logging reorder
    where f  =  return . match
            >=> inline
            >=> return . bindClean
            >=> extractCtx
            >=> argClean
