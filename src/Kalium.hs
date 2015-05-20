{-# LANGUAGE FlexibleContexts #-}
module Kalium (translate) where

import Kalium.Prelude

import Kalium.Nucleus.Vectorize (vectorize)
import Kalium.Nucleus.Scalar.Atomize (atomize)
import Kalium.Nucleus.Scalar.Valueficate (valueficate)
import Kalium.Nucleus.Vector.Match (match)
import Kalium.Nucleus.Vector.Inline (inline, reorder)
import Kalium.Nucleus.Vector.ArgClean (argClean)
import Kalium.Nucleus.Vector.Purify (purify)
import Kalium.Nucleus.Vector.BindClean (bindClean)
import Kalium.Nucleus.Vector.Context (extractCtx)
import Kalium.Nucleus.Vector.Normalize (normalize, denormalize)
import Kalium.Nucleus.Vector.Sanity (sanity_nameUniqueness)
import Language.Haskell.Exts.Pretty (prettyPrint)
import qualified Kalium.Pascal.Parse   as P (parse)
import qualified Kalium.Pascal.Convert as P (convert)
import qualified Kalium.Haskell.Sugar   as H (sugarcoat)
import qualified Kalium.Haskell.Imports as H (imports)
import qualified Kalium.Haskell.Convert as H (convert, Config(..))
import qualified Kalium.Error as E
import qualified Kalium.Nucleus.Scalar.Program as S
import qualified Kalium.Nucleus.Vector.Program as V
import Kalium.Util

sanity_check name f x
    | f x = return x
    | otherwise = throwError (E.Insane name)

nuclear
    :: ( Applicative m
       , MonadError E.Error m
       , MonadNameGen m
       ) => Kleisli' m (S.Program (S.Configuration S.ByType S.Pattern S.Expression))
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
optimize program
    = runWriterT
    $ denormalize <$> closureM optimizeStep (normalize program)

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
