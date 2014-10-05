module Sodium.Haskell.Program where

import qualified Data.Char as C
import qualified Language.Haskell.Exts        as H
import qualified Language.Haskell.Exts.SrcLoc as H

type Name = String

funcDef name args exp = H.FunBind [H.Match H.noLoc (H.Ident name) (map (H.PVar . H.Ident) args) Nothing (H.UnGuardedRhs exp) (H.BDecls [])]

valueDef pat exp = H.PatBind H.noLoc pat Nothing (H.UnGuardedRhs exp) (H.BDecls [])

pureLet []   = id
pureLet defs = H.Let (H.BDecls defs)

expTuple []    = H.Con $ H.Special $ H.UnitCon
expTuple [exp] = exp
expTuple exps  = H.Tuple H.Boxed exps

nameWrap c
    | C.isUpper c = H.Con . H.UnQual . H.Ident
    | C.isLower c || c == '_' = H.Var . H.UnQual . H.Ident
    | c == ':' = H.Con . H.UnQual . H.Symbol
    | otherwise = H.Var . H.UnQual . H.Symbol

access name@(c:_) = nameWrap c name
access [] = error "Null name"

hsName = H.UnQual . H.Ident

multiIf alts = H.MultiIf (uncurry H.IfAlt `map` alts)

doBind H.PWildCard = doExecute
doBind pat = \case
        H.App (H.Var (H.UnQual (H.Ident "return"))) expr
            -> H.LetStmt (H.BDecls [valueDef pat expr])
        expr -> H.Generator H.noLoc pat expr
doExecute  = H.Qualifier


-- MATCHING MAGIC

data Fixity = LFix | RFix | NFix deriving (Eq)
data Level = ALevel Integer Fixity | BLevel | HLevel | SLevel

matchExpression (H.Var op `H.App` expr1 `H.App` expr2)
    | op == H.UnQual (H.Ident "enumFromTo")
        = matchExpression $ expr1 `H.EnumFromTo` expr2
    | isInfix op
        = matchExpression $ H.InfixApp expr1 (H.QVarOp op) expr2
    where isInfix (H.UnQual (H.Ident name)) | name `elem` ["elem", "div", "mod"] = True
          isInfix (H.UnQual (H.Symbol _)) = True
          isInfix _ = False

matchExpression (expr1' `H.App` expr2')
    = H.App lhs rhs where
        expr1 = matchExpression expr1'
        expr2 = matchExpression expr2'
        lhs = case detectLevel expr1 of
                HLevel -> expr1
                BLevel -> expr1
                _ -> H.Paren expr1
        rhs = case detectLevel expr2 of
                HLevel -> expr2
                _ -> H.Paren expr2

matchExpression (H.InfixApp expr1' op expr2') = H.InfixApp lhs op rhs
  where expr1 = matchExpression expr1'
        expr2 = matchExpression expr2'

        defaultHandler n m _
            | m > n = id
            | otherwise = H.Paren
        advHandler n f m g
            | m > n = id
            | f == g && m == n = id
            | otherwise = H.Paren
        (handler1, handler2) = case whatfix op of
            (n, NFix) -> (defaultHandler n,  defaultHandler n)
            (n, LFix) -> (advHandler n LFix, defaultHandler n)
            (n, RFix) -> (defaultHandler n, advHandler n RFix)
        wrap handler expr = case detectLevel expr of
            HLevel -> expr
            BLevel -> expr
            SLevel -> H.Paren expr
            ALevel n fix -> handler n fix expr
        lhs = wrap handler1 expr1
        rhs = wrap handler2 expr2

matchExpression (expr1' `H.EnumFromTo` expr2')
  = H.EnumFromTo (wrap expr1) (wrap expr2) where
      expr1 = matchExpression expr1'
      expr2 = matchExpression expr2'
      wrap expr = case detectLevel expr of
          SLevel -> H.Paren expr
          _ -> expr

matchExpression expr = expr


-- Expects a matched expression
detectLevel :: H.Exp -> Level
detectLevel = \case 
    H.InfixApp _ op _ -> uncurry ALevel (whatfix op)
    H.Var _ -> HLevel
    H.Lit _ -> HLevel
    H.Con _ -> HLevel
    H.Tuple _ _ -> HLevel
    H.Paren _ -> HLevel
    H.App _ _ -> BLevel
    H.EnumFromTo _ _ -> HLevel
    _ -> SLevel

whatfix op = maybe (9, LFix) id (lookup op fixtable)
fixtable = [ (H.QVarOp $ H.UnQual $ H.Symbol "+", (6, LFix))
           , (H.QVarOp $ H.UnQual $ H.Symbol "-", (6, LFix))
           , (H.QVarOp $ H.UnQual $ H.Symbol "*", (7, LFix))
           , (H.QVarOp $ H.UnQual $ H.Symbol "/", (7, LFix))
           , (H.QVarOp $ H.UnQual $ H.Ident "div", (7, LFix))
           , (H.QVarOp $ H.UnQual $ H.Ident "mod", (7, LFix))
           , (H.QVarOp $ H.UnQual $ H.Symbol "==", (4, NFix))
           , (H.QVarOp $ H.UnQual $ H.Symbol "/=", (4, NFix))
           , (H.QVarOp $ H.UnQual $ H.Symbol "<",  (4, NFix))
           , (H.QVarOp $ H.UnQual $ H.Symbol ">",  (4, NFix))
           , (H.QVarOp $ H.UnQual $ H.Ident "elem", (4, NFix))
           , (H.QVarOp $ H.UnQual $ H.Symbol "&&", (3, RFix))
           , (H.QVarOp $ H.UnQual $ H.Symbol "||", (2, RFix))
           ]
