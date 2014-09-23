module Sodium.Haskell.Program where

import Data.Ratio
import qualified Data.Char as C
import qualified Language.Haskell.Exts        as H
import qualified Language.Haskell.Exts.SrcLoc as H

type Name = String

program decls pragmas imports = H.Module H.noLoc (H.ModuleName "Main") pragmas Nothing Nothing imports decls

extensions names = [H.LanguagePragma H.noLoc (map H.Ident names)]

importDecl s = H.ImportDecl H.noLoc (H.ModuleName s) False False Nothing Nothing Nothing

funcDef name args exp = H.FunBind [H.Match H.noLoc (H.Ident name) (map (H.PVar . H.Ident) args) Nothing (H.UnGuardedRhs (matchExpression exp)) (H.BDecls [])]

valueDef pat exp = H.PatBind H.noLoc pat Nothing (H.UnGuardedRhs (matchExpression exp)) (H.BDecls [])

pureLet []   expr = matchExpression expr
pureLet defs expr = H.Let (H.BDecls defs) (matchExpression expr)

lambda pats expr = H.Lambda H.noLoc pats (matchExpression expr)

typed expr ty = H.ExpTypeSig H.noLoc (matchExpression expr) ty

patTuple []     = H.PWildCard
patTuple [name] = H.PVar (H.Ident name)
patTuple names  = H.PTuple H.Boxed . map H.PVar $ map H.Ident names

expTuple []    = H.Con $ H.Special $ H.UnitCon
expTuple [exp] = matchExpression exp
expTuple exps  = H.Tuple H.Boxed (map (matchExpression) exps)

primary = H.Lit

access name
    | null name = error "null name? wtf?"
    | C.isUpper (head name) = (H.Con $ H.UnQual $ H.Ident name)
    | C.isLower (head name) || head name == '_' = (H.Var $ H.UnQual $ H.Ident name)
    | head name == ':' = (H.Con $ H.UnQual $ H.Symbol name)
    | otherwise = (H.Var $ H.UnQual $ H.Symbol name)

quote = H.String
inumber intSection = H.Int $ parseInt intSection
fnumber intSection fracSection = H.Frac $ parseFrac intSection fracSection
enumber intSection fracSection eSign eSection
    = H.Frac $ parseExp intSection fracSection eSign eSection

parseInt :: String -> Integer
parseInt = foldl (\acc c -> fromIntegral (C.digitToInt c) + acc * 10) 0

parseFrac :: String -> String -> Rational
parseFrac intSection fracSection = parseInt (intSection ++ fracSection)
                                 % 10 ^ length fracSection

parseExp :: String -> String -> Bool -> String -> Rational
parseExp intSection fracSection eSign eSection
    = (if eSign then (*) else (/))
        (parseFrac intSection fracSection)
        (10 ^ parseInt eSection)

hsType cs = H.TyCon (H.UnQual (H.Ident cs))
hsUnit = H.TyCon (H.Special H.UnitCon)
hsIO = H.TyApp (hsType "IO")

ifExpression expr1 expr2 expr3 = H.If
    (matchExpression expr1)
    (matchExpression expr2)
    (matchExpression expr3)



doexpr [H.Qualifier exp] = exp
doexpr stmts = H.Do stmts

doBind H.PWildCard expr = doExecute expr
doBind pat expr =
    case matchExpression expr of
        H.App (H.Var (H.UnQual (H.Ident "return"))) expr'
            -> H.LetStmt (H.BDecls [valueDef pat expr'])
        expr' -> H.Generator H.noLoc pat expr'
doExecute  expr = H.Qualifier (matchExpression expr)


-- MATCHING MAGIC

data Fixity = LFix | RFix | NFix deriving (Eq)

data Level = ALevel Integer Fixity | BLevel | HLevel | SLevel

beta = H.App

matchExpression (H.App (H.App (H.Var (H.UnQual (H.Ident "enumFromTo")))expr1)expr2)
    = H.EnumFromTo (wrap $ matchExpression $ expr1) (wrap $ matchExpression $ expr2)
    where wrap expr = case detectLevel expr of
                          SLevel -> H.Paren expr
                          _ -> expr

matchExpression (H.App (H.App (H.Var op) expr1) expr2)
    | isInfix op = matchExpression $ (H.InfixApp (matchExpression $ expr1) (H.QVarOp op) (matchExpression $ expr2))
    where isInfix (H.UnQual (H.Ident name)) | name `elem` ["elem", "div", "mod"] = True
          isInfix (H.UnQual (H.Symbol _)) = True
          isInfix _ = False

matchExpression ((H.App expr1' expr2'))
    = H.App lhs rhs where
        expr1 = matchExpression (expr1')
        expr2 = matchExpression (expr2')
        lhs = case detectLevel expr1 of
                HLevel -> expr1
                BLevel -> expr1
                _ -> H.Paren expr1
        rhs = case detectLevel expr2 of
                HLevel -> expr2
                _ -> H.Paren expr2

matchExpression ((H.InfixApp expr1 op expr2)) = x where
    x = (H.InfixApp lhs op rhs)
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
    wrap handler expr = case detectLevel (expr) of
        HLevel -> expr
        BLevel -> expr
        SLevel -> H.Paren expr
        ALevel n fix -> handler n fix expr
    lhs = wrap handler1 expr1
    rhs = wrap handler2 expr2

matchExpression expr = expr


-- call on matched only!
detectLevel :: H.Exp -> Level
detectLevel = \case 
    (H.InfixApp _ op _) -> uncurry ALevel (whatfix op)
    (H.Var _) -> HLevel
    (H.Lit _) -> HLevel
    (H.Con _) -> HLevel
    (H.Tuple _ _) -> HLevel
    (H.Paren _) -> HLevel
    (H.App _ _) -> BLevel
    (H.EnumFromTo _ _) -> HLevel
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
