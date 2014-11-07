{-# LANGUAGE FlexibleInstances, FlexibleContexts  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
module Sodium.Haskell.Convert (convert, reserved) where

import Data.List (genericReplicate)
import Control.Monad
import Control.Applicative
-- S for Src, D for Dest
import qualified Sodium.Nucleus.Program.Vector as S
import qualified Sodium.Haskell.Program as D
import qualified Language.Haskell.Exts        as H
import qualified Language.Haskell.Exts.SrcLoc as H

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
        return $ H.Module H.noLoc
            (H.ModuleName "Main")
            (extensions ["LambdaCase", "TupleSections", "MultiWayIf"])
            Nothing
            Nothing
            (map importDecl ["Control.Monad", "Control.Applicative"])
            funcDefs
      where extensions names = [H.LanguagePragma H.noLoc (map H.Ident names)]
            importDecl s = H.ImportDecl H.noLoc (H.ModuleName s)
                           False False False Nothing Nothing Nothing

    type Pure S.Program = ()
    pureconv _ = Nothing

transformName :: S.Name -> D.Name
transformName = \case
    S.NameMain -> "main"
    S.NameOp op -> convOp op
    S.Name name -> name
    S.NameSpace namespace name -> namespace ++ "'" ++ transformName name

-- TODO: the complete list of unqualified names
reserved = keywords ++ words
    " show read readLn getLine return foldl \
    \ map filter undefined foldM main       "
keywords = words
      " _ as case class data default deriving do else hiding \
      \   if import in infix infixl infixr instance let      \
      \   module newtype of qualified then type where        "


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

instance Conv S.Type where
    type Norm S.Type = H.Type
    conv = pureconv

    type Pure S.Type = H.Type
    pureconv = \case
        S.TypeInteger -> return $ H.TyCon (D.hsName "Int")
        S.TypeDouble  -> return $ H.TyCon (D.hsName "Double")
        S.TypeBoolean -> return $ H.TyCon (D.hsName "Bool")
        S.TypeChar    -> return $ H.TyCon (D.hsName "Char")
        S.TypeUnit    -> return $ H.TyCon (H.Special H.UnitCon)
        S.TypePair t1 t2  -> (\t1 t2 -> H.TyTuple H.Boxed [t1, t2])
                         <$> pureconv t1 <*> pureconv t2
        S.TypeList S.TypeChar -> return $ H.TyCon (D.hsName "String")
        S.TypeList ts -> H.TyList <$> pureconv ts

instance Conv S.Body where

    type Norm S.Body = H.Exp
    conv (S.Body _ statements resultExpr) = do
        hsStatements <- mapM conv statements
        hsRetValue <- conv resultExpr
        let hsStatement
              = D.doExecute
              $ H.App (D.access "return")
              $ hsRetValue
        return $ case hsStatements ++ [hsStatement] of
            [H.Qualifier expression] -> expression
            statements -> H.Do statements

    type Pure S.Body = H.Exp
    pureconv (S.Body _ statements resultExpr) = do
        hsValueDefs <- mapM pureconv statements
        hsRetValue <- pureconv resultExpr
        return $ D.pureLet hsValueDefs hsRetValue


instance (Conv a, Pure a ~ H.Exp, Norm a ~ H.Exp) => Conv (S.ForCycle a) where
    type Norm (S.ForCycle a) = H.Exp
    conv (S.ForCycle lam argExpr exprRange) = do
        hsRange <- conv exprRange
        hsArgExpr <- conv argExpr
        hsLam <- conv lam
        return $ betaL
            [ D.access "foldM"
            , hsLam
            , hsArgExpr
            , hsRange
            ]

    type Pure (S.ForCycle a) = H.Exp
    pureconv (S.ForCycle lam argExpr exprRange) = do
        hsRange <- pureconv exprRange
        hsArgExpr <- pureconv argExpr
        hsLam <- pureconv lam
        return $ betaL [D.access "foldl", hsLam, hsArgExpr, hsRange]


instance (Conv a, Pure a ~ H.Exp, Norm a ~ H.Exp) => Conv (S.MultiIf a) where

    type Norm (S.MultiIf a) = H.Exp
    conv (S.MultiIf leafs) = do
        let convLeaf (expr, statement)
              =  (,)
             <$> conv expr
             <*> conv statement
        leafGens <- mapM convLeaf leafs
        -- TODO: if the last leaf condition is `True`
        --       then replace it with `otherwise`
        return $ D.multiIf leafGens

    type Pure (S.MultiIf a) = H.Exp
    pureconv (S.MultiIf leafs) = do
        let convLeaf (expr, statement)
              =  (,)
             <$> pureconv expr
             <*> pureconv statement
        leafGens <- mapM convLeaf leafs
        return $ D.multiIf leafGens

instance (Conv a, Pure a ~ H.Exp, Norm a ~ H.Exp) => Conv (S.Bind a) where

    type Norm (S.Bind a) = H.Stmt
    conv (S.Bind S.PWildCard statement) = D.doExecute <$> conv statement
    conv (S.Bind pat statement)
         =  D.doBind
        <$> conv pat
        <*> conv statement

    type Pure (S.Bind a) = H.Decl
    pureconv (S.Bind pat statement)
         =  D.valueDef
        <$> pureconv pat
        <*> pureconv statement

instance Conv S.Statement where

    type Norm S.Statement = H.Exp
    conv (S.Execute (S.NameOp S.OpGetLn) []) = return (D.access "getLine")
    conv (S.Execute (S.NameOp (S.OpReadLn t)) []) = do
        hsType <- conv t
        return $ H.ExpTypeSig H.noLoc
            (D.access "readLn")
            (H.TyCon (D.hsName "IO") `H.TyApp` hsType)
    conv (S.Execute (S.NameOp S.OpPrintLn) args)
        = case args of
            [S.Call (S.NameOp S.OpShow) [arg]] -> H.App (D.access "print") <$> conv arg
            args -> (<$> mapM conv args) $ \case
                [] -> H.App (D.access "putStrLn") (H.Lit (H.String ""))
                hsExprs
                    -> D.matchExpression
                     $ H.App (D.access "putStrLn")
                     $ foldr1 (\x y -> betaL [D.access "++", x, y])
                     $ hsExprs
    conv (S.Execute _ _) = error "Execute..."
    conv (S.ForStatement  forCycle) = conv forCycle
    conv (S.MultiIfStatement multiIf) = conv multiIf
    conv (S.BodyStatement body) = conv body
    conv (S.Assign expr) = H.App (D.access "return") <$> conv expr

    type Pure S.Statement = H.Exp
    pureconv = \case
            S.Assign expr -> pureconv expr
            S.ForStatement forCycle -> pureconv forCycle
            S.MultiIfStatement multiIf -> pureconv multiIf
            S.BodyStatement body -> pureconv body
            _ -> mzero

instance Conv S.Func where
    type Norm S.Func = H.Decl
    conv (S.Func (S.FuncSig S.NameMain paramTypes S.TypeUnit []) _ clBody) = do
        guard $ null paramTypes
        hsBody <- conv clBody
        return $ D.funcDef "main" [] hsBody
    conv fun = pureconv fun

    type Pure S.Func = H.Decl
    pureconv (S.Func (S.FuncSig name _ _ _) params clBody)
         = D.funcDef (transformName name) paramNames
        <$> pureconv clBody
        where paramNames = map transformName params

instance (Conv a, Norm a ~ H.Exp, Pure a ~ H.Exp) => Conv (S.Lambda a) where
    type Norm (S.Lambda a) = H.Exp
    conv (S.Lambda pats act) = H.Lambda H.noLoc
        <$> mapM conv pats
        <*> conv act

    type Pure (S.Lambda a) = H.Exp
    pureconv (S.Lambda pats act) = H.Lambda H.noLoc
        <$> mapM pureconv pats
        <*> pureconv act


betaL = foldl1 H.App

instance Conv S.Pattern where
    type Norm S.Pattern = H.Pat
    conv = pureconv

    type Pure S.Pattern = H.Pat
    pureconv S.PWildCard = return H.PWildCard
    pureconv (S.PAccess name i) = H.PVar . H.Ident <$> pureconv (Name name i)
    pureconv (S.PTuple pats) = H.PTuple H.Boxed <$> mapM pureconv pats


instance Conv S.Expression where

    type Norm S.Expression = H.Exp
    conv = pureconv

    type Pure S.Expression = H.Exp
    pureconv expr = D.matchExpression <$> convexpr expr

convexpr :: S.Expression -> Maybe H.Exp
convexpr (S.Primary lit) = return (convlit lit)
convexpr (S.Access name i) = D.access <$> pureconv (Name name i)
convexpr (S.Call op exprs) = do
    hsExprs <- mapM convexpr exprs
    return $ betaL (D.access (transformName op) : hsExprs)
convexpr (S.Tuple exprs) = D.expTuple <$> mapM convexpr exprs
convexpr (S.Fold op expr range) = do
    hsArgExpr <- convexpr expr
    hsRange <- convexpr range
    return $ betaL [D.access "foldl", D.access (transformName op), hsArgExpr, hsRange]
convexpr (S.MultiIfExpression multiIf) = pureconv multiIf

convlit :: S.Literal -> H.Exp
convlit = \case
    S.Lit S.STypeInteger n -> (if n < 0 then H.Paren else id) $ H.Lit (H.Int  n)
    S.Lit S.STypeDouble  x -> (if x < 0 then H.Paren else id) $ H.Lit (H.Frac x)
    S.Lit S.STypeChar    c -> H.Lit $ H.Char c
    S.Lit S.STypeBoolean a -> H.Con $ H.UnQual $ H.Ident (if a then "True" else "False")
    S.Lit S.STypeUnit   () -> H.Con $ H.Special H.UnitCon
    S.Lit (S.STypeList S.STypeChar) cs -> H.Lit $ H.String cs
    S.Lit (S.STypeList t) xs -> H.List $ map (\x -> convlit (S.Lit t x)) xs
    S.Lit (S.STypePair t1 t2) (x1, x2)
         -> H.Tuple H.Boxed [convlit (S.Lit t1 x1), convlit (S.Lit t2 x2)]

convOp :: S.Operator -> D.Name
convOp = \case
    S.OpNegate   -> "negate"
    S.OpShow     -> "show"
    S.OpProduct  -> "product"
    S.OpSum      -> "sum"
    S.OpAnd'     -> "and"
    S.OpOr'      -> "or"
    S.OpAdd      -> "+"
    S.OpSubtract -> "-"
    S.OpMultiply -> "*"
    S.OpDivide   -> "/"
    S.OpDiv      -> "div"
    S.OpMod      -> "mod"
    S.OpMore     -> ">"
    S.OpLess     -> "<"
    S.OpEquals   -> "=="
    S.OpXor      -> "/="
    S.OpAnd      -> "&&"
    S.OpOr       -> "||"
    S.OpNot      -> "not"
    S.OpElem     -> "elem"
    S.OpRange    -> "enumFromTo"
    S.OpId       -> "id"
    S.OpFst      -> "fst"
    S.OpSnd      -> "snd"
    S.OpPrintLn  -> "print"
    S.OpGetLn    -> "getLine"
    S.OpReadLn _ -> "readLn"
    S.OpSingleton -> "return"
    S.OpIntToDouble -> "fromIntegral"
