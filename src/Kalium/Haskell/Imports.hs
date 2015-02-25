{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Kalium.Haskell.Imports where

import Kalium.Prelude
import Kalium.Util
import Data.Data
import qualified Data.Set as S
import qualified Data.Map as M
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.SrcLoc (noLoc)

imports :: Module -> Module
imports (Module srcLoc name pragmas wtext exportSpec importDecls decls) =
    let importDecls' = importDecls ++ map importDecl
            (itoList (ModuleName "Prelude" `M.delete` modules))
        modules = gcollect decls
    in unqual (Module srcLoc name pragmas wtext exportSpec importDecls' decls)
        `runReader` mempty

importDecl (moduleName, names) =
    ImportDecl noLoc moduleName
    False False False Nothing Nothing (Just (False, IAbs <$> toList names))

gcollect :: Data a => a -> Map ModuleName (Set Name)
gcollect = (`execState` mempty) . go where
  go :: (Data d, Applicative m, MonadState (Map ModuleName (Set Name)) m)
     => EndoKleisli' m d
  go a | Just (Qual moduleName name) <- cast a
       = a <$ modify (M.insertWith S.union moduleName (S.singleton name))
       | otherwise = gmapM go a

class Unqual a where
    unqual :: (Applicative m, MonadReader (Set Name) m) => EndoKleisli' m a

class    Scoping a    where scoping :: a -> Set Name
instance Scoping Name where scoping = S.singleton

instance Scoping Pat  where
    scoping = \case
        PWildCard -> mempty
        PVar name -> scoping name
        PTuple _ pats -> scoping pats
        PatTypeSig _ pat _ -> scoping pat
        pat -> error ("unsupported pat: " ++ show pat)

instance Scoping Decl where
    scoping = \case
        FunBind matches -> scoping matches
        PatBind _ pat _ _ -> scoping pat
        TypeSig _ names _ -> scoping names
        decl -> error ("unsupported decl: " ++ show decl)

instance Scoping Stmt where
    scoping = \case
        Qualifier _ -> mempty
        Generator _ pat _ -> scoping pat
        LetStmt binds -> scoping binds
        RecStmt stmts -> scoping stmts

instance Scoping Binds where
    scoping = \case
        IPBinds {} -> error "not supported: IPBinds"
        BDecls decls -> scoping decls

instance Scoping Match where
    scoping (Match _ name _ _ _ _) = scoping name

instance (Scoping a, Foldable f) => Scoping (f a) where
    scoping = foldMap scoping

unqualLocalize
    :: (Applicative m, MonadReader (Set Name) m, Scoping p)
    => p -> Endo' (m a)
unqualLocalize (scoping -> p) = local (mappend p)

instance (Unqual a, Traversable f) => Unqual (f a) where
    unqual = traverse unqual

instance Unqual Module where
    unqual (Module srcLoc name pragmas wtext exportSpec importDecls            decls)
         =  Module srcLoc name pragmas wtext exportSpec importDecls <$> unqual decls
         & unqualLocalize decls

instance Unqual Decl where
    unqual = \case
        FunBind matches -> FunBind <$> unqual matches
        PatBind srcLoc pat rhs binds
            -> PatBind srcLoc <$> unqual pat <*> unqual rhs <*> unqual binds
             & unqualLocalize pat
        TypeSig srcLoc names ty
            -> TypeSig srcLoc names <$> unqual ty
             & unqualLocalize names
        decl -> error ("unsupported decl: " ++ show decl)

instance Unqual Match where
    unqual (Match srcLoc name pats ty rhs binds)
        = Match srcLoc name
            <$> unqual pats
            <*> unqual ty
            <*> unqual rhs
            <*> unqual binds

instance Unqual Binds where
    unqual = \case
        IPBinds {} -> error "not supported: IPBinds"
        BDecls decls -> BDecls <$> unqual decls
                      & unqualLocalize decls

instance Unqual Rhs where
    unqual = \case
        UnGuardedRhs exp -> UnGuardedRhs <$> unqual exp
        GuardedRhss rhss -> GuardedRhss  <$> unqual rhss

instance Unqual GuardedRhs where
    unqual (GuardedRhs srcLoc            stmts            exp)
         =  GuardedRhs srcLoc <$> unqual stmts <*> unqual exp
         & unqualLocalize stmts

instance Unqual Stmt where
    unqual = \case
        Generator srcLoc pat exp
            -> Generator srcLoc <$> unqual pat <*> unqual exp
             & unqualLocalize pat
        Qualifier exp -> Qualifier <$> unqual exp
        LetStmt binds -> LetStmt <$> unqual binds & unqualLocalize binds
        RecStmt stmts -> RecStmt <$> unqual stmts & unqualLocalize stmts

instance Unqual Exp where
    unqual = \case
        exp@Lit{} -> return exp
        Var qname -> Var <$> unqual qname
        Con qname -> Con <$> unqual qname
        List exps -> List <$> unqual exps
        MultiIf rhss -> MultiIf <$> unqual rhss
        If exp1 exp2 exp3 -> If <$> unqual exp1 <*> unqual exp2 <*> unqual exp3
        EnumFromTo exp1 exp2 -> EnumFromTo <$> unqual exp1 <*> unqual exp2
        Tuple boxed exps -> Tuple boxed <$> unqual exps
        Paren exp -> Paren <$> unqual exp
        App x y -> App <$> unqual x <*> unqual y
        Lambda srcLoc pats exp
             -> Lambda srcLoc
            <$> unqual pats
            <*>(unqual exp & unqualLocalize pats)
        InfixApp x op y -> InfixApp <$> unqual x <*> unqual op <*> unqual y
        RightSection op exp -> RightSection <$> unqual op <*> unqual exp
        Do stmts -> Do <$> unqual stmts & unqualLocalize stmts
        Let binds exp
            -> Let <$> unqual binds <*> unqual exp
             & unqualLocalize binds
        exp -> error ("unsupported exp: " ++ show exp)

instance Unqual Type where
    unqual = \case
        TyFun ty1 ty2 -> TyFun <$> unqual ty1 <*> unqual ty2
        TyApp ty1 ty2 -> TyApp <$> unqual ty1 <*> unqual ty2
        TyTuple boxed ty -> TyTuple boxed <$> unqual ty
        TyList ty -> TyList <$> unqual ty
        TyCon qname -> TyCon <$> unqual qname
        ty -> error ("unsupported type: " ++ show ty)

instance Unqual Pat where
    unqual = \case
        pat@PWildCard -> return pat
        pat@PVar{}    -> return pat
        PTuple boxed pats -> PTuple boxed <$> unqual pats
        PatTypeSig srcLoc pat ty ->
            PatTypeSig srcLoc <$> unqual pat <*> unqual ty
        pat -> error ("unsupported pat: " ++ show pat)

instance Unqual QOp where
    unqual = \case
        QVarOp qname -> QVarOp <$> unqual qname
        QConOp qname -> QConOp <$> unqual qname

instance Unqual QName where
    unqual qname@(Qual _ name) =
        asks (elem name) <&> bool (UnQual name) qname
    unqual qname = return qname

