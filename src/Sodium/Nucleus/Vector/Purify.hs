{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Sodium.Nucleus.Vector.Purify where

import Sodium.Prelude
import Sodium.Util

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Control.Dependent as Dep

import Sodium.Nucleus.Vector.Program
import Sodium.Nucleus.Vector.Recmap
import Sodium.Nucleus.Vector.Name

purify :: (Applicative m, MonadNameGen m) => EndoKleisli' m Program
purify program = do
    p1s <- execWriterT $ itraverse funcPurify (program ^. programFuncs)
    let p2s = resolve' p1s
        infoGroups = (map.map) fst (Dep.group p2s)
    return (substitute infoGroups program)

substitute :: [[PurifyInfo]] -> Endo' Program
substitute = tryApply . (flip.foldM.flip) substituteSCC

substituteSCC :: [PurifyInfo] -> EndoKleisli' Maybe Program
substituteSCC infoGroup program =
    let impureNames = S.unions [ S.singleton name | PurifyInfo name _ _ _ <- infoGroup ]
        program' = withPure
        dangling = program' `mentions` impureNames
        withoutImpure = S.foldr
            (\name -> programFuncs %~ M.delete name) program impureNames
        replacedCalls = withoutImpure & over recmapped purifyExpression
        withPure = replacedCalls & programFuncs %~ M.union pureFuncs
        pureFuncs = M.unions [ M.singleton name' func | PurifyInfo _ _ name' func <- infoGroup ]
    in if dangling then Nothing else Just program'
  where
    purifyExpression :: Endo' Expression
    purifyExpression = foldr (.) id $ map (tryApply.appPurify) infoGroup

data Request = Request Name Int
    deriving (Eq, Ord)
data P1 = P1 Name Int Name (Pairs Name Name -> Maybe (Func, Set Request))

data PurifyInfo = PurifyInfo Name Int Name Func

instance Dep.Dependent (PurifyInfo, Set Request) where
    type Name (PurifyInfo, Set Request) = Request
    provides (fst -> PurifyInfo _ arity name' _) = Request name' arity
    depends = snd

resolve :: Monad m => (m a -> a -> m b) -> m a -> m b
resolve f ma = ma >>= f ma

resolve' = resolve getGen
  where
    getGen ps (P1 name arity name' gen) = maybeToList $ do
        (func, reqs) <- gen (map getNames ps)
        return (PurifyInfo name arity name' func, reqs)
    getNames = \(P1 name _ name' _) -> (name, name')

funcPurify
    :: ( Applicative m
       , MonadNameGen m
       , MonadWriter [P1] m )
    => Name -> Func -> m ()
funcPurify (NameSpecial _) _ = return ()
funcPurify name (Func ty a) = do
    ( (ps,tys) , (a',ty') ) <- typeDrivenUnlambda ty a
    case ty' of
        TypeApp1 TypeTaint ty'' -> do
            let ty1 = tyfun tys ty''
                arity = length tys
                gen = expForcePurify' a'
                    & mapped . mapped . _1 %~ (Func ty1 . lambda ps)
            name' <- alias name
            tell [P1 name arity name' gen]
        _ -> return ()

expForcePurify
    :: ( Applicative m
       , MonadReader (Pairs Name Name) m
       , MonadError () m
       , MonadWriter (Set Request) m)
    => EndoKleisli' m Expression
expForcePurify = \case
    Taint a -> return a
    Follow p x a -> Into p <$> expForcePurify x <*> expForcePurify a
    Into p x a -> Into p x <$> expForcePurify a
    AppOp3 OpIf xElse xThen cond
         -> AppOp3 OpIf
        <$> expForcePurify xElse
        <*> expForcePurify xThen
        <*> pure cond
    (unbeta -> (Access name : es)) -> do
        let arity = length es
        name' <- asks (lookup name) >>= maybe (throwError ()) return
        tell (S.singleton (Request name' arity))
        return (beta (Access name' : es))
    _ -> throwError ()

expForcePurify'
    :: Expression -> Pairs Name Name -> Maybe (Expression, Set Request)
expForcePurify' = fmap runError . runReaderT . runWriterT . expForcePurify
    where runError = either (const Nothing) Just . runExcept

typeDrivenUnlambda
    :: (Applicative m, MonadNameGen m)
    => Type -> Expression -> m ( ([Pattern],[Type]) , (Expression,Type) )
typeDrivenUnlambda ty a =
  let (tys, ty') = untyfun ty
      ( ps,  a') = unlambda a
      tysLength = length tys
      psLength = length  ps
  in if | tysLength < psLength -> do
            error "lambda-abstraction with non-function type"

        | tysLength > psLength -> do
            let tys' = drop psLength tys
            b <- etaExpand tys' a -- TODO: or is it a'?
            typeDrivenUnlambda ty b

        | otherwise -> return ( (ps,tys) , (a',ty') )

appPurify :: PurifyInfo -> Expression -> Maybe Expression
appPurify (PurifyInfo name arity name' _) e
    | (Access op:es) <- unbeta e, op == name
    , arity == length es
    = Just $ Taint . beta $ (Access name' : es)
appPurify _ _ = Nothing
