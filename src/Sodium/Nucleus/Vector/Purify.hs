{-# LANGUAGE FlexibleContexts #-}
module Sodium.Nucleus.Vector.Purify where

import Sodium.Prelude
import Sodium.Util

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Graph as G
import Data.Tree (flatten)

import Sodium.Nucleus.Vector.Program
import Sodium.Nucleus.Vector.Recmap
import Sodium.Nucleus.Vector.Name

purify :: (Applicative m, MonadNameGen m) => EndoKleisli' m Program
purify program = do
    p1s <- execWriterT $ itraverse funcPurify (program ^. programFuncs)
    let p2s = resolve p1s
        infoGroups = fulfil p2s
    return (foldr substitute program infoGroups)

substitute :: [PurifyInfo] -> Endo' Program
substitute infoGroup program =
    let impureNames = S.unions [ S.singleton name | PurifyInfo name _ _ _ <- infoGroup ]
        program' = withPure
        dangling = program' `mentions` impureNames
        withoutImpure = S.foldr
            (\name -> programFuncs %~ M.delete name) program impureNames
        replacedCalls = withoutImpure & over recmapped purifyExpression
        withPure = replacedCalls & programFuncs %~ M.union pureFuncs
        pureFuncs = M.unions [ M.singleton name' func | PurifyInfo _ _ name' func <- infoGroup ]
    in if dangling then program else program'
        -- TODO: stop further substitutions
  where
    purifyExpression :: Endo' Expression
    purifyExpression = foldr (.) id $ map (tryApply.appPurify) infoGroup

data Request = Request Name Int
    deriving (Eq, Ord)
data P1 = P1 Name Int Name (Map Name Name -> Maybe (Func, Set Request))
data P2 = P2 Name Int Name Func (Set Request)

data PurifyInfo = PurifyInfo Name Int Name Func

resolve :: [P1] -> [P2]
resolve p1s =
    let cell (P1 name _ name' _) = M.singleton name name'
        table = p1s & map cell & M.unions
    in p1s >>= resolve1 table

resolve1 :: Map Name Name -> P1 -> [P2]
resolve1 table (P1 name arity name' gen)
    = maybeToList $ uncurry (P2 name arity name') `fmap` gen table

fulfil :: [P2] -> [[PurifyInfo]]
fulfil p2s = groups where

    (graph, lookupVertex, _) = G.graphFromEdges
        $ flip map p2s
        $ \(P2 name arity name' func reqs) ->
            ( PurifyInfo name arity name' func
            , Request name arity
            , S.toList reqs )

    groups = (fmap.fmap) (view _1 . lookupVertex)
           $  fmap flatten
           $ G.scc graph

funcPurify
    :: ( Applicative m
       , MonadNameGen m
       , MonadWriter [P1] m )
    => Name -> Func -> m ()
funcPurify (NameSpecial _) _ = return ()
funcPurify name (Func ty a) = do
    ( (ps,tys) , (a',ty') ) <- typeDrivenUnlambda ty a
    case ty' of
        TypeTaint ty'' -> do
            let ty1 = tyfun tys ty''
                arity = length tys
                gen = expForcePurify' a'
                    & mapped . mapped . _1 %~ (Func ty1 . lambda ps)
            name' <- alias name
            tell [P1 name arity name' gen]
        _ -> return ()

expForcePurify
    :: ( Applicative m
       , MonadReader (Map Name Name) m
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
        name' <- asks (M.lookup name) >>= maybe (throwError ()) return
        tell (S.singleton (Request name' arity))
        return (beta (Access name' : es))
    _ -> throwError ()

expForcePurify'
    :: Expression -> Map Name Name -> Maybe (Expression, Set Request)
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
            b <- etaExpand tys' a'
            typeDrivenUnlambda ty b

        | otherwise -> return ( (ps,tys) , (a',ty') )

appPurify :: PurifyInfo -> Expression -> Maybe Expression
appPurify (PurifyInfo name arity name' _) e
    | (Access op:es) <- unbeta e, op == name
    , arity == length es
    = Just $ Taint . beta $ (Access name' : es)
appPurify _ _ = Nothing
