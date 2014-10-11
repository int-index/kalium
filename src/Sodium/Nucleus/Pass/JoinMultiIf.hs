module Sodium.Nucleus.Pass.JoinMultiIf (joinMultiIf) where

import Data.List
import Control.Lens
import Sodium.Nucleus.Program.Vector
import Sodium.Nucleus.Recmap.Vector
import Sodium.Nucleus.Pass.Compute (recursively)

import Sodium.Util (tryApply)

joinMultiIf :: Program -> Program
joinMultiIf = over recmapped (recursively joinMultiIfExpression)
            . over recmapped joinMultiIfStatement

joinMultiIfExpression :: Expression -> Expression
joinMultiIfExpression = tryApply joinMultiIf
    where joinMultiIf (MultiIfExpression (MultiIf leafs)) = merge leafs
          joinMultiIf _ = Nothing

          merge :: [(Expression, Expression)] -> Maybe Expression
          merge (leaf:leafs) = case leaf of
            (cond, a) | cond == Primary (LitBoolean True) -> Just a
            _ -> fmap (MultiIfExpression . MultiIf . match leaf) (merge leafs)
          merge [] = Nothing

          match leaf = \case
            MultiIfExpression (MultiIf leafs') -> (leaf:leafs')
            expr -> [leaf, (Primary (LitBoolean True), expr)]

joinMultiIfStatement :: Statement -> Statement
joinMultiIfStatement = tryApply
    $ \statement ->  statement
                 ^? _MultiIfStatement
                 >>= matchExpression
                 <&> Assign

matchExpression :: MultiIf Statement -> Maybe Expression
matchExpression multiIf = do
    exprLeafs <- (traversed . _2) (preview _Assign) (multiIf ^. multiIfLeafs)
    return $ MultiIfExpression $ MultiIf exprLeafs
