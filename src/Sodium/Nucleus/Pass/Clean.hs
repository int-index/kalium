module Sodium.Nucleus.Pass.Clean (clean) where

import Control.Applicative
import Control.Lens hiding (Index, Fold)
import Control.Monad.Reader
import qualified Data.Map as M
import Data.Bool
import Data.List
import Sodium.Nucleus.Program.Vector
import Sodium.Nucleus.Recmap.Vector

clean :: Program -> Program
clean = over recmapped (cleanBody . cleanRetBody)

cleanBody :: Body -> Body
cleanBody body = (bodyVars %~ M.filterWithKey cc) body where
    cc name _ = runReader (checkRef $ bodyComponents body) name


cleanRetBody :: Body -> Body
cleanRetBody body = body & bodyBinds .~ binds where
    wild [] = []
    wild (bind:binds) = [cleanRetBind usage bind]
        where check = runReader $ checkRef (binds, body ^. bodyResults)
              usage = map (check . fst) (bind ^. bindIndices)
    binds = (tails $ body ^. bodyBinds) >>= wild

cleanRetBind :: [Bool] -> Bind -> Bind
cleanRetBind usage bind = maybe bind id (runReaderT (eliminate bind) usage)

keep :: [a] -> ReaderT [Bool] Maybe [a]
keep xs = do
    usage <- ask
    guard $ length xs == length usage
    return $ concat
           $ zipWith (\used b -> if used then [b] else []) usage xs

class Eliminate a where
    eliminate :: a -> ReaderT [Bool] Maybe a

instance Eliminate Bind where
    eliminate = bindStatement eliminate >=> bindIndices keep

instance Eliminate Statement where
    eliminate
        =  _BodyStatement    eliminate
       >=> _MultiIfStatement eliminate
       >=> _Execute          (const mzero)
       >=> _ForStatement     (const mzero)
       >=> _Assign           (const mzero)

instance Eliminate Body where
    eliminate = bodyResults keep

instance Eliminate MultiIfBranch where
    eliminate  = (multiIfLeafs . traversed . _2) eliminate
              >=> multiIfElse eliminate


class CheckRef a where
    checkRef :: a -> Reader Name Bool


-- Helper instances

instance CheckRef a => CheckRef [a] where
    checkRef as = or <$> traversed checkRef as

instance (CheckRef a, CheckRef b) => CheckRef (a, b) where
    checkRef (a, b) = (||) <$> checkRef a <*> checkRef b


-- Actual instances

instance CheckRef Expression where
    checkRef = \case
        Primary _ -> return False
        Access name' _ -> do
            name <- ask
            return (name == name')
        Call _ exprs -> -- Check the operator?
            checkRef exprs
        Fold _ exprs range -> checkRef (exprs, range)

instance CheckRef Statement where
    checkRef = \case
        Execute _ exprs -> checkRef exprs
        Assign expr -> checkRef expr
        BodyStatement body -> checkRef body
        ForStatement forCycle -> checkRef forCycle
        MultiIfStatement multiIfBranch -> checkRef multiIfBranch

instance CheckRef Bind where
    checkRef = checkRef . view bindStatement

instance CheckRef ForCycle where
    checkRef forCycle = do
        shadowed <- shadowedBy (forCycle ^.. forArgIndices . traversed . _1)
        let base = (forCycle ^. forRange, forCycle ^. forArgExprs)
        let unsh = bool [forCycle ^. forAction] [] shadowed
        checkRef (base, unsh)

instance CheckRef MultiIfBranch where
    checkRef multiIfBranch = checkRef 
        (multiIfBranch ^. multiIfLeafs, multiIfBranch ^. multiIfElse)

bodyComponents body = (body ^. bodyResults, body ^. bodyBinds)

instance CheckRef Body where
    checkRef body = do
        shadowed <- shadowedBy (body ^. bodyVars . to M.keys)
        checkRef $ bool [bodyComponents body] [] shadowed

shadowedBy :: [Name] -> Reader Name Bool
shadowedBy names = (`elem` names) <$> ask
