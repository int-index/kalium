{-# LANGUAGE FlexibleInstances, FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies #-}
module Sodium.Haskell.Convert (convert, reserved) where

import Data.List (intercalate)
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
        funcDefs <- concat <$> mapM conv funcs
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

-- TODO: the complete list of unqualified names
reserved = keywords ++ words
    " show read readLn getLine return foldl \
    \ map filter undefined foldM main       "
keywords = words
      " _ as case class data default deriving do else hiding \
      \   if import in infix infixl infixr instance let      \
      \   module newtype of qualified then type where        "


transformName :: S.Name1 S.IndexTag -> D.Name
transformName = \case
    S.NameOp op -> convOp op
    S.Name1 ns tag -> let s = intercalate "'" ns in case tag of
        S.IndexTag n -> s ++ "'" ++ show n
        S.ImmutableTag -> s ++ "'" ++ "const"
        S.GlobalTag -> s

instance Conv (S.Name1 S.IndexTag) where
    type Norm (S.Name1 S.IndexTag) = D.Name
    conv = pureconv

    type Pure (S.Name1 S.IndexTag) = D.Name
    pureconv = return . transformName

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

instance Conv (S.Body S.Statement) where

    type Norm (S.Body S.Statement) = H.Exp
    conv (S.Body statements resultStatement) = do
        hsStatements <- mapM conv statements
        hsStatement  <- D.doExecute <$> conv resultStatement
        return $ case hsStatements ++ [hsStatement] of
            [H.Qualifier expression] -> expression
            statements -> H.Do statements

    type Pure (S.Body S.Statement) = H.Exp
    pureconv (S.Body statements statement) = do
        hsValueDefs <- mapM pureconv statements
        hsRetValue <- pureconv statement
        return $ D.pureLet hsValueDefs hsRetValue


instance Conv S.ForCycle where
    type Norm S.ForCycle = H.Exp
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

    type Pure S.ForCycle = H.Exp
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
            [S.Call (S.OpAccess S.OpShow) arg] -> H.App (D.access "print") <$> conv arg
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
    conv (S.LambdaStatement lam) = conv lam

    type Pure S.Statement = H.Exp
    pureconv = \case
            S.Assign expr -> pureconv expr
            S.ForStatement forCycle -> pureconv forCycle
            S.MultiIfStatement multiIf -> pureconv multiIf
            S.BodyStatement body -> pureconv body
            S.LambdaStatement lam -> pureconv lam
            _ -> mzero

instance Conv S.Func where
    type Norm S.Func = [H.Decl]
    conv (S.Func (S.FuncSig (S.NameOp S.OpMain) [] S.TypeUnit) statement) = do
        hsStatement <- conv statement
        hsType <- conv S.TypeUnit
        let hsName = H.Ident (convOp S.OpMain)
        let sig = H.TypeSig H.noLoc [hsName]
                (H.TyCon (D.hsName "IO") `H.TyApp` hsType)
        return [sig, D.funcDef hsName [] hsStatement]
    conv fun = pureconv fun

    type Pure S.Func = [H.Decl]
    pureconv (S.Func (S.FuncSig name paramTypes retType) statement) = do
        hsStatement <- pureconv statement
        let hsName = H.Ident (transformName name)
        hsType <- foldr1 H.TyFun <$> mapM conv (paramTypes ++ [retType])
        let sig = H.TypeSig H.noLoc [hsName] hsType
        return [sig, D.funcDef hsName [] hsStatement]

instance (Conv a, Norm a ~ H.Exp, Pure a ~ H.Exp) => Conv (S.Lambda a) where
    type Norm (S.Lambda a) = H.Exp
    conv (S.Lambda [] act) = conv act
    conv (S.Lambda pats act) = H.Lambda H.noLoc
        <$> mapM conv pats
        <*> conv act

    type Pure (S.Lambda a) = H.Exp
    pureconv (S.Lambda [] act) = pureconv act
    pureconv (S.Lambda pats act) = H.Lambda H.noLoc
        <$> mapM pureconv pats
        <*> pureconv act


betaL = foldl1 H.App

instance Conv S.Pattern where
    type Norm S.Pattern = H.Pat
    conv = pureconv

    type Pure S.Pattern = H.Pat
    pureconv S.PUnit = return (H.PTuple H.Boxed [])
    pureconv S.PWildCard = return H.PWildCard
    pureconv (S.PAccess name) = H.PVar . H.Ident <$> pureconv name
    pureconv (S.PTuple pat1 pat2) = H.PTuple H.Boxed <$> mapM pureconv [pat1, pat2]


instance Conv S.Expression where

    type Norm S.Expression = H.Exp
    conv = pureconv

    type Pure S.Expression = H.Exp
    pureconv expr = D.matchExpression <$> convexpr expr

convexpr :: S.Expression -> Maybe H.Exp
convexpr (S.Primary lit) = return (convlit lit)
convexpr (S.Access name) = D.access <$> pureconv name
convexpr (S.Call  (S.OpAccess S.OpSingleton) expr)
    = H.List <$> mapM convexpr [expr]
convexpr (S.Call2 (S.OpAccess S.OpPair) expr1 expr2)
    = D.expTuple <$> mapM convexpr [expr1, expr2]
convexpr (S.Call expr1 expr2) = H.App <$> convexpr expr1 <*> convexpr expr2
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
    S.OpFold     -> "foldl"
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
    S.OpPair     -> ","
    S.OpFst      -> "fst"
    S.OpSnd      -> "snd"
    S.OpPrintLn  -> "print"
    S.OpGetLn    -> "getLine"
    S.OpReadLn _ -> "readLn"
    S.OpSingleton -> "return"
    S.OpIntToDouble -> "fromIntegral"
    S.OpUndefined -> "undefined"
    S.OpMain -> "main"
