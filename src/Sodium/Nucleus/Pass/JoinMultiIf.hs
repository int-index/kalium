module Sodium.Nucleus.Pass.JoinMultiIf (joinMultiIf) where

import Control.Lens
import Sodium.Nucleus.Program.Vector
import Sodium.Nucleus.Recmap.Vector

import Sodium.Util (tryApply)

joinMultiIf :: Program -> Program
joinMultiIf = over recmapped joinMultiIfExpression
            . over recmapped joinMultiIfStatement

joinMultiIfExpression :: Expression -> Expression
joinMultiIfExpression
    = _MultiIfExpression %~ tryApply joinMultiIf
    where joinMultiIf multiIf
            =  multiIf ^? multiIfElse . _MultiIfExpression
           <&> over multiIfLeafs (view multiIfLeafs multiIf ++)

joinMultiIfStatement :: Statement -> Statement
joinMultiIfStatement = tryApply
    $ \statement ->  statement
                 ^? _MultiIfStatement
                 >>= matchExpression
                 <&> Assign

matchExpression :: MultiIf Statement -> Maybe Expression
matchExpression multiIf = do
    exprLeafs <- (traversed . _2) (preview _Assign) (multiIf ^. multiIfLeafs)
    exprElse  <- multiIf ^? multiIfElse . _Assign
    return $ MultiIfExpression $ MultiIf exprLeafs exprElse
