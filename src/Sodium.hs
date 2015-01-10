{-# LANGUAGE FlexibleContexts #-}
module Sodium (translate) where

import Sodium.Prelude

import Sodium.Nucleus.Vectorize (vectorize)
import Sodium.Nucleus.Scalar.Atomize (atomize)
import Sodium.Nucleus.Scalar.Valueficate (valueficate)
import Sodium.Nucleus.Vector.Match (match)
import Sodium.Nucleus.Vector.Inline (inline, reorder)
import Sodium.Nucleus.Vector.ArgClean (argClean)
import Sodium.Nucleus.Vector.Purify (purify)
import Sodium.Nucleus.Vector.BindClean (bindClean)
import Sodium.Nucleus.Vector.Context (extractCtx)
import Sodium.Nucleus.Vector.Sanity (sanity_nameUniqueness)
import Language.Haskell.Exts.Pretty (prettyPrint)
import qualified Sodium.Pascal.Parse   as P (parse)
import qualified Sodium.Pascal.Convert as P (convert)
import qualified Sodium.Haskell.Sugar   as H (sugarcoat)
import qualified Sodium.Haskell.Imports as H (imports)
import qualified Sodium.Haskell.Convert as H (convert, Config(..))
import qualified Sodium.Error as E
import qualified Sodium.Nucleus.Scalar.Program as S
import qualified Sodium.Nucleus.Vector.Program as V
import Sodium.Util

sanity_check name f x
    | f x = return x
    | otherwise = throwError (E.Insane name)

nuclear
    :: ( Applicative m
       , MonadError E.Error m
       , MonadNameGen m
       ) => Kleisli' m (S.Program S.ByType S.Pattern S.Expression)
                       (V.Program, TranslationLog)
nuclear
     =  atomize
    >=> atomize . valueficate
    >=> vectorize
    >=> sanity_check "Name uniqueness" sanity_nameUniqueness
    >=> optimize

translate
    :: (Applicative m, MonadError E.Error m)
    => Bool -> String -> m ([String], String)
translate configPatSig src = do
    pas <- P.parse src
    ((optimal, log), nameTags) <- (`runRenameT` 0) $ P.convert pas >>= nuclear
    let hsConfig = H.Config { H.configPatSig = configPatSig }
    let sweeten = H.imports . H.sugarcoat . H.convert hsConfig nameTags
    return ( map (prettyPrint . sweeten) log
           , prettyPrint (sweeten optimal)
           )

type TranslationLog = [V.Program]

optimize
    :: (Applicative m, MonadNameGen m)
    => V.Program -> m (V.Program, TranslationLog)
optimize program = runWriterT (closureM optimizeStep program)

logging :: MonadWriter [a] m => LensLike' m a a
logging f x = tell [x] >> f x

optimizeStep
    :: (Applicative m, MonadWriter TranslationLog m, MonadNameGen m)
    => EndoKleisli' m V.Program
optimizeStep = closureM (logging f) >=> logging reorder
    where f  =  return . match
            >=> inline
            >=> return . bindClean
            >=> extractCtx
            >=> argClean
            >=> purify
