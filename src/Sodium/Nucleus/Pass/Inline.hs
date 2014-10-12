{-# LANGUAGE DataKinds #-}
module Sodium.Nucleus.Pass.Inline (inline) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Lens hiding (Index, Fold)
import Sodium.Nucleus.Program.Vector
import Sodium.Nucleus.Recmap.Vector

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
    -- TODO: pattern merging
    merge :: Pattern -> State Body Pattern
    merge (PTuple pats)
        | topsm <- pats ^.. traversed . to (preview _PAccess)
        , Just tops <- sequence topsm
        , expr <- map (review _Access) tops
        = return (PTuple pats)
    merge pat = return pat

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
