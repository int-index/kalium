{-# LANGUAGE DataKinds #-}
module Sodium.Nucleus.Pass.Inline (inline) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import qualified Data.Set as S
import Control.Lens hiding (Index, Fold)
import Sodium.Nucleus.Program.Vector
import Sodium.Nucleus.Recmap.Vector

import Debug.Trace

inline :: Program -> Program
inline = over recmapped inlineBody

inlineBody :: Body -> Body
inlineBody = evalState unconsBind where
    unconsBind :: State Body Body
    unconsBind = uses bodyBinds uncons >>= maybe get go
    go (bind', binds) = do
        bodyBinds .= binds
        bind <- bindPattern merge bind'
        elim bind <$> get >>= \case
            Just body -> put body >> unconsBind
            Nothing   -> over bodyBinds (bind:) <$> unconsBind
    elim bind |  bind ^. bindPattern   . to (==PWildCard)
              && bind ^. bindStatement . to noExec
              = return
    elim bind = \body' -> do
        Bind (PAccess name i) (Assign expr) <- return bind
        let (body, count)
              = runWriter
              $ flip runReaderT ((name, i), expr)
              $ recmapped inl body'
        when (expr ^? _Access == Nothing) $ guard (count <= 1)
        return body
    merge :: Pattern -> State Body Pattern
    merge p@(PTuple pat1 pat2)
        | topsm <- [pat1, pat2] ^.. traversed . to (preview _PAccess)
        , Just [top1, top2] <- sequence topsm
        -- TODO: generate a name!
        , top <- (NameSpace "merge" (Name "a"), Immutable)
        = do let model =  Tuple (review _Access top1) (review _Access top2)
             let pat   = _PAccess # top
             let expr  =  _Access # top
             gets (over recmapped (model `subst` expr))
                >>= \case body | eb1 <- execWriter (body  & recmapped exprBound)
                               , eb2 <- execWriter (model & recExpr   exprBound)
                               , S.null (join traceShow $ eb1 `S.intersection` eb2)
                            -> put body >> return pat
                          _ -> return p
    merge pat = return pat

subst :: Expression -> Expression -> (Expression -> Expression)
subst model expr expr'
    | model == expr' = expr
    | otherwise = expr'

exprBound :: Expression -> Writer (S.Set (Name, Index)) Expression
exprBound expr = do
    case expr of
        Access name i -> tell (S.singleton (name, i))
        _ -> return ()
    return expr


-- check if a statement contains any side-effects
noExec :: Statement -> Bool
noExec = r where
    r = maybe False (const True) . recmapped failExec
    failExec :: Statement -> Maybe Statement
    failExec = \case
        Execute _ _ -> Nothing
        statement -> Just statement

type Inline = ReaderT ((Name, Index), Expression) (Writer (Sum Integer))

inl :: Expression -> Inline Expression
inl expr'@(Access name' j) = do
        (name, expr) <- ask
        if name == (name', j)
            then tell (Sum 1) >> return expr
            else return expr'
inl expr' = return expr'
