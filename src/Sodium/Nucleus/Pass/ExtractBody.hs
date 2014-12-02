module Sodium.Nucleus.Pass.ExtractBody (extractBody) where

import Control.Lens
import Control.Monad
import Sodium.Nucleus.Program.Vector
import Sodium.Nucleus.Recmap
import Sodium.Nucleus.Pattern
import Sodium.Util (tryApply)

extractBody :: Program -> Program
extractBody = over recmapped (tryApply bodyPurify . tryApply bodyMatch)

bodyMatch :: Statement -> Maybe Statement
bodyMatch (BodyStatement body)
    | null (body ^. bodyBinds) = return (body ^. bodyResult)
    | otherwise = do
        [bind] <- return (body ^. bodyBinds)
        Assign expr <- return (body ^. bodyResult)
        guard $ expMatch (bind ^. bindPattern) expr
        return (bind ^. bindStatement)
bodyMatch _ = Nothing

bodyPurify :: Statement -> Maybe Statement
bodyPurify statement = do
    BodyStatement (Body binds result) <- Just statement
    binds'  <- traverse bindPurify binds
    result' <- statementPurify result
    return $ Assign $ BodyExpression (Body binds' result')
    where statementPurify (Assign expr) = Just expr
          statementPurify _ = Nothing
          bindPurify (Bind pat statement) = Bind pat
            `fmap` statementPurify statement
