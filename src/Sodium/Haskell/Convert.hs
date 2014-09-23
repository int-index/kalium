{-# LANGUAGE FlexibleInstances, FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies #-}
module Sodium.Haskell.Convert (convert) where

import Data.List (genericReplicate)
import Control.Monad
import Control.Applicative
import qualified Data.Map as M
-- S for Src, D for Dest
import qualified Sodium.Nucleus.Program.Vector as S
import qualified Sodium.Haskell.Program as D
import qualified Language.Haskell.Exts as H

convert :: S.Program -> H.Module
convert = maybe (error "Sodium.Haskell.Convert") id . conv

class Conv s where

    type Norm s :: *
    conv     :: s -> Maybe (Norm s)

    type Pure s :: *
    pureconv :: s -> Maybe (Pure s)

instance Conv S.Program where
    type Norm S.Program = H.Module
    conv (S.Program funcs) = do
        funcDefs <- mapM conv funcs
        return $ D.program funcDefs
            (D.extensions ["LambdaCase", "TupleSections"])
            (map D.importDecl ["Control.Monad", "Control.Applicative"])

    type Pure S.Program = ()
    pureconv _ = Nothing

transformName :: S.Name -> D.Name
transformName = \case
    S.NameMain -> "main"
    S.Name cs
        -> (if reserved cs
            then ("_'"++)
            else id) cs
    S.NameGen u -> "_'" ++ show u
    S.NameUnique name -> transformName name ++ "'_"
    where reserved = flip elem
               [ "let", "show", "read", "readLn", "getLine", "return", "foldl"
               , "map", "filter", "undefined", "main", "import", "_"]

data Name = Name S.Name S.Index
    deriving (Eq)

instance Conv Name where
    type Norm Name = D.Name
    conv = pureconv

    type Pure Name = D.Name
    pureconv (Name name i) = case i of
        S.Index n -> return
            $ transformName name ++ genericReplicate n '\''
        S.Immutable -> return $ "const'" ++ transformName name
        S.Uninitialized -> return "undefined"

instance Conv S.ClType where
    type Norm S.ClType = H.Type
    conv = pureconv

    type Pure S.ClType = H.Type
    pureconv = return . \case
        S.ClInteger -> D.hsType "Int"
        S.ClDouble  -> D.hsType "Double"
        S.ClBoolean -> D.hsType "Bool"
        S.ClString  -> D.hsType "String"
        S.ClVoid -> D.hsUnit

instance Conv S.Body where

    type Norm S.Body = H.Exp
    conv (S.Body _ statements resultExprs) = do
        hsStatements <- mapM conv statements
        hsRetValues <- mapM conv resultExprs
        let hsStatement
              = D.doExecute
              $ D.beta (D.access "return")
              $ D.expTuple hsRetValues
        return $ D.doexpr (hsStatements ++ [hsStatement])

    type Pure S.Body = H.Exp
    pureconv (S.Body _ statements resultExprs) = msum
        [ do name1 <- case resultExprs of
                   [S.Access name i] -> return $ Name name i
                   _ -> mzero
             let appToLast f xs = case reverse xs of
                   (x:xs') -> (, reverse xs') <$> f x
                   _ -> mzero
             let extractIndex = \case
                   [(name, i)] -> return (Name name i)
                   _ -> mzero
             let convStatement (S.Bind indices statement)
                    =  (,)
                   <$> extractIndex indices
                   <*> case statement of
                       S.Assign expr -> pureconv expr
                       S.ForStatement forCycle -> pureconv forCycle
                       S.MultiIfStatement multiIfBranch -> pureconv multiIfBranch
                       S.BodyStatement body -> pureconv body
                       _ -> mzero
             ((name2, hsExpr), statements) <- appToLast convStatement statements
             guard $ name1 == name2
             hsValueDefs <- mapM pureconv statements
             return $ D.pureLet hsValueDefs hsExpr
        , do hsValueDefs <- mapM pureconv statements
             hsRetValues <- mapM pureconv resultExprs
             return $ D.pureLet hsValueDefs (D.expTuple hsRetValues)
        ]


instance Conv S.ForCycle where
    type Norm S.ForCycle = H.Exp
    conv (S.ForCycle argIndices argExprs name exprRange clBody) = do
        hsRange <- conv exprRange
        hsArgExpr <- D.expTuple <$> mapM conv argExprs
        hsFoldLambda <- conv (FoldLambda argIndices name) <*> conv clBody
        return $ betaL
            [ D.access "foldM"
            , hsFoldLambda
            , hsArgExpr
            , hsRange
            ]

    type Pure S.ForCycle = H.Exp
    pureconv (S.ForCycle argIndices argExprs name exprRange clBody) = do
        hsRange <- pureconv exprRange
        hsArgExpr <- D.expTuple <$> mapM pureconv argExprs
        hsFoldLambda
            <-  pureconv (FoldLambda argIndices name)
            <*> pureconv clBody
        return $ betaL [D.access "foldl", hsFoldLambda, hsArgExpr, hsRange]


instance Conv S.MultiIfBranch where

    type Norm S.MultiIfBranch = H.Exp
    conv (S.MultiIfBranch leafs statementElse) = do
        let convLeaf (expr, statement)
              =  D.ifExpression
             <$> conv expr
             <*> conv statement
        leafGens <- mapM convLeaf leafs
        hsStatementElse <- conv statementElse
        return $ foldr ($) hsStatementElse leafGens

    type Pure S.MultiIfBranch = H.Exp
    pureconv (S.MultiIfBranch leafs statementElse) = do
        let convLeaf (expr, statement)
              =  D.ifExpression
             <$> pureconv expr
             <*> pureconv statement
        leafGens <- mapM convLeaf leafs
        hsStatementElse <- pureconv statementElse
        return $ foldr ($) hsStatementElse leafGens

instance Conv S.Bind where

    type Norm S.Bind = H.Stmt
    conv (S.Bind [] statement) = D.doExecute <$> conv statement
    conv (S.Bind retIndices statement)
         =  D.doBind
        <$> (D.patTuple <$> conv (IndicesList retIndices))
        <*> conv statement

    type Pure S.Bind = H.Decl
    pureconv (S.Bind retIndices statement)
         =  D.valueDef
        <$> (D.patTuple <$> pureconv (IndicesList retIndices))
        <*> pureconv statement

instance Conv S.Statement where

    type Norm S.Statement = H.Exp
    conv (S.Execute (S.OpReadLn t) [])
        | t == S.ClString = return (D.access "getLine")
        | otherwise = do
                hsType <- conv t
                return $ D.access "readLn" `D.typed` D.hsIO hsType
    conv (S.Execute S.OpPrintLn args)
        = case args of
            [S.Call S.OpShow [arg]] -> D.beta (D.access "print") <$> conv arg
            args -> (<$> mapM conv args) $ \case
                [] -> D.beta (D.access "putStrLn") (D.primary (D.quote ""))
                hsExprs
                    -> D.beta (D.access "putStrLn")
                     $ foldl1 (\x y -> betaL [D.access "++", x, y])
                     $ hsExprs
    conv (S.Execute op args) = error ("Execute " ++ show op ++ " " ++ show args)
    conv (S.ForStatement  forCycle) = conv forCycle
    conv (S.MultiIfStatement multiIfBranch) = conv multiIfBranch
    conv (S.BodyStatement body) = conv body
    conv (S.Assign expr) = D.beta (D.access "return") <$> conv expr

    type Pure S.Statement = H.Exp
    pureconv = \case
            S.Assign expr -> pureconv expr
            S.ForStatement forCycle -> pureconv forCycle
            S.MultiIfStatement multiIfBranch -> pureconv multiIfBranch
            S.BodyStatement body -> pureconv body
            _ -> mzero

instance Conv S.Func where
    type Norm S.Func = H.Decl
    conv (S.Func (S.FuncSig S.NameMain params S.ClVoid) clBody) = do
        guard $ M.null params
        hsBody <- conv clBody
        return $ D.funcDef "main" [] hsBody
    conv fun = pureconv fun

    type Pure S.Func = H.Decl
    pureconv (S.Func (S.FuncSig name params _) clBody)
         = D.funcDef (transformName name) paramNames
        <$> pureconv clBody
        where paramNames = map transformName (M.keys params)

data FoldLambda = FoldLambda S.IndicesList S.Name

instance Conv FoldLambda where
    type Norm FoldLambda = H.Exp -> H.Exp
    conv = pureconv

    type Pure FoldLambda = H.Exp -> H.Exp
    pureconv (FoldLambda indices name) = do
        hsNames <- conv (IndicesList indices)
        hsName  <- conv (Name name S.Immutable)
        return $ D.lambda [D.patTuple hsNames, D.patTuple [hsName]]


betaL = foldl1 D.beta

newtype IndicesList = IndicesList S.IndicesList

instance Conv IndicesList where
    type Norm IndicesList = [D.Name]
    conv = pureconv

    type Pure IndicesList = [D.Name]
    pureconv (IndicesList xs) = mapM (pureconv . uncurry Name) xs


instance Conv S.Expression where

    type Norm S.Expression = H.Exp
    conv = pureconv

    type Pure S.Expression = H.Exp
    pureconv (S.Primary prim) = return $ case prim of
        S.Quote cs -> D.primary (D.quote cs)
        S.INumber intSection -> D.primary (D.inumber intSection)
        S.FNumber intSection fracSection
            -> D.primary (D.fnumber intSection fracSection)
        S.ENumber intSection fracSection eSign eSection
            -> D.primary (D.enumber intSection fracSection eSign eSection)
        S.BTrue  -> D.access "True"
        S.BFalse -> D.access "False"
        S.Void   -> D.expTuple []
    pureconv (S.Access name i) = D.access <$> pureconv (Name name i)
    pureconv (S.Call op exprs) = do
        hsExprs <- mapM pureconv exprs
        return $ betaL (D.access (convOp op) : hsExprs)
    pureconv (S.Fold op exprs range) = do
        hsArgExpr <- D.expTuple <$> mapM pureconv exprs
        hsRange <- pureconv range
        return $ betaL [D.access "foldl", D.access (convOp op), hsArgExpr, hsRange]

convOp = \case
    S.OpNegate -> "negate"
    S.OpShow -> "show"
    S.OpProduct -> "product"
    S.OpSum -> "sum"
    S.OpAnd' -> "and"
    S.OpOr' -> "or"
    S.OpAdd -> "+"
    S.OpSubtract -> "-"
    S.OpMultiply -> "*"
    S.OpDivide -> "/"
    S.OpMore -> ">"
    S.OpLess -> "<"
    S.OpEquals -> "=="
    S.OpAnd -> "&&"
    S.OpOr -> "||"
    S.OpElem -> "elem"
    S.OpRange -> "enumFromTo"
    S.OpId -> "id"
    S.OpName name -> transformName name
    S.OpPrintLn  -> "print"
    S.OpReadLn _ -> "readLn"
