module Sodium.Nucleus.Pass.BindClean (bindClean) where

import Control.Monad
import Control.Lens
import Data.Monoid
import Data.List

import Sodium.Nucleus.Program.Vector
import Sodium.Nucleus.Recmap.Vector

bindClean :: Program -> Program
bindClean = over recmapped bindCleanBody

bindCleanBody :: Body -> Body
bindCleanBody = bodyBinds . traversed %~ bindCleanBind

bindCleanBind :: Bind -> Bind
bindCleanBind bind
    = maybe bind id
    $ getFirst . mconcat . map First
    $ map (subst bind)
    $ patClean (bind ^. bindPattern)

subst :: Bind -> (Pattern, Cleaner) -> Maybe Bind
subst bind (pat, cleaner)
    = bind & bindPattern .~ pat
           & bindStatement (cleanRet cleaner)

patClean :: Pattern -> [(Pattern, Cleaner)]
patClean p@PWildCard = [(p, wildCleaner)]
patClean (PTuple pats) = concat (unfoldr go 0) where
    go n = do
        (pre, cur:post) <- Just (splitAt n pats)
        let wrap (PWildCard, _)  = [(PTuple (pre++post), tupWildCleaner n)]
            wrap (cur', _) = [(PTuple (pre++cur':post) -- TODO: implement it
                             , error "Nested pattern cleaning not implemented")]
        return (patClean cur >>= wrap, succ n)
patClean _ = []

type Cleaner = Expression -> Maybe Expression

wildCleaner :: Cleaner
wildCleaner _ = Just (Primary LitUnit)

tupWildCleaner :: Int -> Cleaner
tupWildCleaner n expr = do
    Tuple  exprs  <- Just expr
    (pre, _:post) <- Just (splitAt n exprs)
    return $ Tuple (pre ++ post)

class CleanRet a where
    cleanRet :: Cleaner -> a -> Maybe a

instance CleanRet Expression where
    cleanRet = id

instance CleanRet Body where
    cleanRet cc = bodyResult (cleanRet cc)

instance CleanRet MultiIfBranch where
    cleanRet cc  = (multiIfLeafs . traversed . _2) (cleanRet cc)
                >=> multiIfElse (cleanRet cc)

instance CleanRet ForCycle where
    cleanRet cc = forAction (cleanRet cc)

instance CleanRet Statement where
    cleanRet cc
         =  _Assign  (cleanRet cc)
        >=> _Execute (const mzero)
        >=> _ForStatement     (cleanRet cc)
        >=> _MultiIfStatement (cleanRet cc)
        >=> _BodyStatement    (cleanRet cc)
