{-# LANGUAGE FlexibleContexts #-}
module Kalium (translate, Cache, updateCache, extractCache) where

import Kalium.Prelude
import Control.Exception

import Kalium.Nucleus.Vectorize (vectorize)
import Kalium.Nucleus.Scalar.Atomize (atomize)
import Kalium.Nucleus.Scalar.Valueficate (valueficate)
import Kalium.Nucleus.Vector.Match (match, match')
import Kalium.Nucleus.Vector.Inline (inline, reorder)
import Kalium.Nucleus.Vector.ArgClean (argClean)
import Kalium.Nucleus.Vector.RetClean (retClean)
import Kalium.Nucleus.Vector.Purify (purify)
import Kalium.Nucleus.Vector.BindClean (bindClean)
import Kalium.Nucleus.Vector.Context (extractCtx)
import Kalium.Nucleus.Vector.Normalize (normalize, denormalize)
import Kalium.Nucleus.Vector.Sanity (sanity_nameUniqueness)
import Language.Haskell.Exts.Pretty (prettyPrint)
import qualified Kalium.Pascal.Program as P (Program)
import qualified Kalium.Pascal.Parse   as P (parse)
import qualified Kalium.Pascal.Convert as P (convert)
import qualified Kalium.Haskell.Sugar   as H (sugarcoat)
import qualified Kalium.Haskell.Imports as H (imports)
import qualified Kalium.Haskell.Convert as H (convert, Config(..))
import qualified Kalium.Nucleus.Scalar.Program as S
import qualified Kalium.Nucleus.Vector.Program as V
import Kalium.Util

import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.Rename

instance Exception ErrorInsane
data ErrorInsane = ErrorInsane String
    deriving (Show)

sanity_check name f x
    | f x = return x
    | otherwise = (throwError.SomeException) (ErrorInsane name)

nuclear
    :: ( MonadError SomeException m
       , MonadNameGen m
       ) => Kleisli' m (S.Complex S.Program) (V.Program, TranslationLog)
nuclear
     =  atomize
    >=> atomize . valueficate
    >=> vectorize
    >=> sanity_check "Name uniqueness" sanity_nameUniqueness
    >=> optimize

rnuclear pas = (`runRenameT` 0) (P.convert pas >>= nuclear)

translate
    :: (MonadError SomeException m)
    => Bool -> String -> m ([String], String)
translate configPatSig src = do
    pas <- P.parse src
    ((optimal, log), nameTags) <- rnuclear pas
    let hsConfig = H.Config { H.configPatSig = configPatSig }
    let sweeten = H.imports . H.sugarcoat . H.convert hsConfig nameTags
    return ( map (prettyPrint . sweeten) log
           , prettyPrint (sweeten optimal) )

data Cache
    = Cache_PascalParseError String SomeException
    | Cache_HaskellGenError String P.Program SomeException
    | Cache_Success String P.Program (Map Integer String, V.Program) H.Config String

extractCache :: Cache -> Either SomeException String
extractCache = \case
    Cache_PascalParseError _ e -> Left e
    Cache_HaskellGenError _ _ e -> Left e
    Cache_Success _ _ _ _ r -> Right r

updateCache :: Bool -> String -> Maybe Cache -> Cache
updateCache configPatSig src =
  let hsConfig = H.Config { H.configPatSig = configPatSig }
  in \case
    Nothing -> case P.parse src of
        Left (e :: SomeException) -> Cache_PascalParseError src e
        Right pas -> withPas hsConfig pas
    Just cache -> case cache of
      Cache_PascalParseError src' _ ->
        if src == src' then cache else updateCache configPatSig src Nothing
      Cache_HaskellGenError src' pas' e' ->
        if src == src' then cache else
          case P.parse src of
            Left (e :: SomeException) -> Cache_PascalParseError src e
            Right pas -> if pas == pas'
                then Cache_HaskellGenError src pas e'
                else withPas hsConfig pas
      Cache_Success src' pas' opt' hsConfig' _ ->
        let keep | hsConfig == hsConfig' = cache
                 | otherwise = withOptimal hsConfig pas' opt'
        in if src == src' then keep
           else case P.parse src of
               Left (e :: SomeException) -> Cache_PascalParseError src e
               Right pas -> if pas == pas' then keep else withPas hsConfig pas

  where
    withPas hsConfig pas = case rnuclear pas of
        Left (e :: SomeException) -> Cache_HaskellGenError src pas e
        Right ((optimal, _log), nameTags) -> withOptimal hsConfig pas (nameTags,optimal)
    withOptimal hsConfig pas (nameTags,optimal) =
        let sweet = H.imports . H.sugarcoat
                  $ H.convert hsConfig nameTags optimal
        in Cache_Success src pas (nameTags,optimal) hsConfig (prettyPrint sweet)


type TranslationLog = [V.Program]

optimize
    :: (MonadNameGen m)
    => V.Program -> m (V.Program, TranslationLog)
optimize program
    = runWriterT
    $ denormalize <$> closureM optimizeStep (normalize program)

logging :: MonadWriter [a] m => LensLike' m a a
logging f x = tell [x] >> f x

optimizeStep
    :: (MonadWriter TranslationLog m, MonadNameGen m)
    => EndoKleisli' m V.Program
optimizeStep =  closureM g
            >=> closureM (logging f)
            >=> logging reorder
    where g  =  return . match' >=> inline >=> return . bindClean
          f  =  return . match
            >=> inline
            >=> return . bindClean
            >=> extractCtx
            >=> argClean
            >=> retClean
            >=> purify
