module Sodium.Nucleus.IOMagic (uncurse) where

import Control.Monad.Reader
import Control.Lens
import qualified Data.Map as M
import Data.Monoid
import Sodium.Nucleus.Program.Scalar
import Sodium.Nucleus.Recmap.Scalar

data Error
    = NoAccess Name
    deriving (Show)
uncurse :: Program -> Program
uncurse program
    = either (error . show) id
    . flip runReaderT M.empty
    $ recmap rm program
    where rm = localizer (local . M.union) <> recmapper uncurseStatement

uncurseStatement :: Statement -> ReaderT Vars (Either Error) Statement
uncurseStatement = _Execute
    $ \(mres, name, args) -> case name of
        NameOp (OpReadLn _) -> case (mres, args) of
            (Nothing, [Atom (Access name)])
                 -> lookupType name
                <&> \t -> (Just name, NameOp (OpReadLn t), [])
            _ -> error "IOMagic supports only single-value read operations"
        NameOp OpPrintLn
             -> mapM uncurseArgument args
            <&> \args -> (mres, name, args)
        _ -> return (mres, name, args)

uncurseArgument = \case
    -- TODO: apply `show` only to non-String
    -- expressions as soon as typecheck is implemented
    Atom (Access name)
         -> lookupType name
        <&> \case
            TypeString -> Atom (Access name)
            _ -> Call (NameOp OpShow) [Atom (Access name)]
    expr -> return expr

lookupType name = do
    vars <- ask
    lift $ maybe (Left $ NoAccess name) Right (M.lookup name vars)
