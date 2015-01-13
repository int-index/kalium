{-# LANGUAGE FlexibleInstances #-}
module Sodium.Haskell.Sugar where

import Sodium.Prelude
import Sodium.Util (closureM)

import Sodium.Haskell.Common
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.SrcLoc (noLoc)

sugarcoat :: Module -> Module
sugarcoat = runIdentity . closureM sugar

class Sugar a where
    sugar :: (Applicative m, Monad m) => a -> m a

instance (Sugar a, Traversable f) => Sugar (f a) where
    sugar = traverse sugar

instance Sugar Module where
    sugar (Module srcLoc name pragmas wtext exportSpec importDecls           decls)
        =  Module srcLoc name pragmas wtext exportSpec importDecls <$> sugar decls

instance Sugar Decl where
    sugar = \case
        FunBind matches -> FunBind <$> sugar matches
        PatBind srcLoc pat rhs binds
            -> PatBind srcLoc pat <$> sugar rhs <*> sugar binds
        decl@TypeSig{} -> pure decl
        decl -> error ("unsupported decl: " ++ show decl)

instance Sugar Match where
    sugar (Match srcLoc name pats ty rhs binds) = do
        (pats', rhs') <- sugar rhs <&> \case
            UnGuardedRhs (Lambda _ pats' exp)
                 -> (pats ++ pats', UnGuardedRhs exp)
            rhs' -> (pats, rhs')
        Match srcLoc name pats' ty rhs' <$> sugar binds

instance Sugar Binds where
    sugar = \case
        IPBinds {} -> error "not supported: IPBinds"
        BDecls decls -> BDecls <$> sugar decls

sugarUniverseStrip a = expStripParen ParensUniverse <$> sugar a

instance Sugar Rhs where
    sugar = \case
        UnGuardedRhs (MultiIf rhss) -> sugar (GuardedRhss rhss)
        UnGuardedRhs exp -> UnGuardedRhs <$> sugarUniverseStrip exp
        GuardedRhss rhss -> GuardedRhss  <$> sugar rhss

instance Sugar GuardedRhs where
    sugar (GuardedRhs srcLoc           stmts           exp)
        =  GuardedRhs srcLoc <$> sugar stmts <*> sugarUniverseStrip exp

instance Sugar Stmt where
    sugar = \case
        Generator srcLoc pat exp
            -> Generator srcLoc pat <$> sugarUniverseStrip exp
        Qualifier exp -> Qualifier <$> sugarUniverseStrip exp
        LetStmt binds -> LetStmt <$> sugar binds
        RecStmt stmts -> RecStmt <$> sugar stmts

pattern App2 op x y = op `App` x `App` y
pattern App3 op x y z = op `App` x `App` y `App` z

instance Sugar Exp where
    sugar = fmap expMatch . \case
        exp@Var{} -> return exp
        exp@Con{} -> return exp
        exp@Lit{} -> return exp
        List exps -> List <$> sugar exps
        MultiIf rhss -> MultiIf <$> sugar rhss
        If exp1 exp2 exp3 -> If <$> sugar exp1 <*> sugar exp2 <*> sugar exp3
        EnumFromTo exp1 exp2 -> EnumFromTo <$> sugar exp1 <*> sugar exp2
        Tuple boxed exps -> Tuple boxed <$> sugar exps
        Paren exp  -> Paren <$> sugar exp
        App x y -> App <$> sugar x <*> sugar y
        Lambda srcLoc pats exp -> Lambda srcLoc pats <$> sugar exp
        InfixApp x op y -> InfixApp <$> sugar x <*> pure op <*> sugar y
        RightSection op exp -> RightSection op <$> sugar exp
        Do stmts -> Do <$> sugar stmts
        Let binds exp -> Let <$> sugar binds <*> sugar exp
        exp -> error ("unsupported exp: " ++ show exp)

expMatch
    = expStripParen ParensAtom
    . expMatchInfix
    . expMatchIf
    . expJoinLambda
    . expJoinList
    . expAppSection
    . expDoMatch
    . expLetMatch

isInfix op = lookup op fixtable /= Nothing

expMatchIf = \case
    App3 (Var op) x y z | HsIdent "Data.Bool" "bool" <- op -> If z y x
    If expCond expThen expElse
        | MultiIf rhss <- expElse
            -> MultiIf (GuardedRhs noLoc [Qualifier expCond] expThen : rhss)
        | If expCond' expThen' expElse' <- expElse
            -> MultiIf [ GuardedRhs noLoc [Qualifier expCond ] expThen
                       , GuardedRhs noLoc [Qualifier expCond'] expThen'
                       , GuardedRhs noLoc [Qualifier trueCond] expElse'
                       ] where trueCond = Var (HsIdent "Prelude" "otherwise")
    exp -> exp

expMatchInfix = \case
    App2 (Con op) x y -> case op of
        Special (TupleCon boxed _) -> Tuple boxed [x, y]
        _ -> Paren (InfixApp (Paren x) (QConOp op) (Paren y))
    App2 (Var op) x y | HsIdent "Prelude" "enumFromTo" <- op
        -> EnumFromTo x y
    App2 (Var op) x y | isInfix (QVarOp op)
        -> Paren (InfixApp (Paren x) (QVarOp op) (Paren y))
    exp -> exp

expStripParen outerLevel = \case
    Paren exp | expLevel exp `lesserLevel` outerLevel -> exp

    App (Paren x) y | expLevel x `lesserLevel` ParensApp -> App x y
    App x (Paren y) | expLevel y `lesserLevel` ParensApp -> App x y

    InfixApp (Paren (InfixApp l_lhs l_op l_rhs)) op rhs
        | (Leftfix, l_n) <- whatfix l_op
        , (Leftfix,   n) <- whatfix   op
        , l_n == n
        -> InfixApp (InfixApp l_lhs l_op l_rhs) op rhs

    InfixApp lhs op (Paren (InfixApp r_lhs r_op r_rhs))
        | (Rightfix, r_n) <- whatfix r_op
        , (Rightfix,   n) <- whatfix   op
        , r_n == n
        -> InfixApp lhs op (InfixApp r_lhs r_op r_rhs)

    InfixApp lhs op rhs | (fx, n) <- whatfix op
        -> InfixApp
            (expStripParen (ParensInfix fx n) lhs)
            op
            (expStripParen (ParensInfix fx n) rhs)
    Do stmts ->
        let strip = \case
              Qualifier (Paren exp) -> Qualifier exp
              stmt -> stmt
        in Do (map strip stmts)

    Let binds exp -> Let binds (expStripParen ParensUniverse exp)

    Tuple boxed exps -> Tuple boxed (expStripParen ParensUniverse `map` exps)
    List        exps -> List        (expStripParen ParensUniverse `map` exps)

    If exp1 exp2 exp3 -> If
        (expStripParen ParensUniverse exp1)
        (expStripParen ParensUniverse exp2)
        (expStripParen ParensUniverse exp3)

    exp -> exp

expJoinLambda = \case
    Lambda srcLoc pats (Lambda _ pats' exp) -> Lambda srcLoc (pats ++ pats') exp
    exp -> exp

expJoinList = \case
    List xs | Just cs <- charList xs -> Lit (String cs)
    InfixApp x (QConOp (Special Cons)) (List xs) -> List (x:xs)
    InfixApp (Lit (Char c)) (QConOp (Special Cons)) (Lit (String cs))
        -> Lit (String (c:cs))
    exp -> exp

charList = traverse $ \case
    Lit (Char c) -> Just c
    _ -> Nothing

expAppSection = \case
    App (RightSection (QVarOp op) y) x -> App2 (Var op) x y
    App (RightSection (QConOp op) y) x -> App2 (Con op) x y
    exp -> exp

expDoMatch = \case
    App2 (Var (HsSymbol "Prelude" ">>=")) x (Lambda srcLoc [pat] a)
        -> Do [Generator srcLoc pat x, Qualifier a]
    App2 (Var (HsSymbol "Prelude" ">>")) x y
        -> Do [Qualifier x, Qualifier y]
    Do [Qualifier exp] -> exp
    Do stmts -> Do (stmts >>= expandStmt) where
        expandStmt = \case
            Qualifier (Do stmts) -> stmts
            stmt -> [stmt]
    exp -> exp

expLetMatch = \case
    App (Lambda srcLoc [pat] a) x ->
        let decl = PatBind srcLoc pat (UnGuardedRhs x) (BDecls [])
        in Let (BDecls [decl]) a
    Let (BDecls decls) (Let (BDecls decls') a)
        -> Let (BDecls (decls ++ decls')) a
    exp -> exp

data Fixity = Nonfix | Leftfix | Rightfix
    deriving (Eq)

data ParensLevel
    = ParensAtom
    | ParensApp
    | ParensInfix Fixity Int
    | ParensUniverse
    deriving (Eq)

lesserLevel :: ParensLevel -> ParensLevel -> Bool
lesserLevel l ParensUniverse = l /= ParensUniverse
lesserLevel ParensAtom     l = l /= ParensAtom
lesserLevel ParensApp ParensInfix{} = True
lesserLevel (ParensInfix _ n1) (ParensInfix _ n2) = n1 > n2
lesserLevel _ _ = False

expLevel :: Exp -> ParensLevel
expLevel = \case
    Paren{} -> ParensAtom
    Var{} -> ParensAtom
    Con{} -> ParensAtom
    Lit{} -> ParensAtom
    List{} -> ParensAtom
    EnumFromTo{} -> ParensAtom
    App{} -> ParensApp
    InfixApp _ op _ -> ParensInfix `uncurry` whatfix op
    _ -> ParensUniverse

whatfix op = maybe (Leftfix, 9) id (lookup op fixtable)
fixtable = [ (QVarOp $ HsSymbol "Prelude" ".",  (Rightfix, 9))
           , (QVarOp $ HsSymbol "Prelude" "!!", (Leftfix,  9))
           , (QVarOp $ HsSymbol "Prelude" "+", (Leftfix, 6))
           , (QVarOp $ HsSymbol "Prelude" "-", (Leftfix, 6))
           , (QVarOp $ HsSymbol "Prelude" "*", (Leftfix, 7))
           , (QVarOp $ HsSymbol "Prelude" "/", (Leftfix, 7))
           , (QVarOp $ HsSymbol "Prelude" "div", (Leftfix, 7))
           , (QVarOp $ HsSymbol "Prelude" "mod", (Leftfix, 7))
           , (QVarOp $ HsSymbol "Prelude" "++", (Rightfix, 5))
           , (QVarOp $ HsSymbol "Control.Applicative" "<$", (Leftfix, 4))
           , (QVarOp $ HsSymbol "Prelude" "==", (Nonfix, 4))
           , (QVarOp $ HsSymbol "Prelude" "/=", (Nonfix, 4))
           , (QVarOp $ HsSymbol "Prelude" "<",  (Nonfix, 4))
           , (QVarOp $ HsSymbol "Prelude" ">",  (Nonfix, 4))
           , (QVarOp $ HsSymbol "Prelude" "<=", (Nonfix, 4))
           , (QVarOp $ HsSymbol "Prelude" ">=", (Nonfix, 4))
           , (QVarOp $ HsSymbol "Prelude" "elem", (Nonfix, 4))
           , (QVarOp $ HsSymbol "Prelude" "&&", (Rightfix, 3))
           , (QVarOp $ HsSymbol "Prelude" "||", (Rightfix, 2))
           , (QVarOp $ HsSymbol "Prelude" ">>=", (Leftfix, 1))
           , (QVarOp $ HsSymbol "Prelude" ">>",  (Leftfix, 1))
           ]
