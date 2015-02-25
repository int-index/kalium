{-# LANGUAGE FlexibleContexts #-}
module Kalium.Haskell.Convert (convert, Config(..)) where

import Kalium.Prelude

import Data.Char (isAlphaNum, isLetter)
import qualified Data.Map as M
import qualified Data.Set as S

import Kalium.Haskell.Common
import qualified Kalium.Nucleus.Vector.Program as Vec
import qualified Kalium.Nucleus.Vector.Pattern as Vec
import qualified Kalium.Nucleus.Vector.Operator as VecOp
import qualified Language.Haskell.Exts        as H
import qualified Language.Haskell.Exts.SrcLoc as H

convert :: Config -> Map Integer String -> Vec.Program -> H.Module
convert = convProgram

data Config = Config
    { configPatSig :: Bool
    }

type T m =
    ( Applicative m
    , MonadReader (Set Vec.Name) m
    , MonadState (Map Integer (Either String H.Name)) m )
type C m = (Applicative m, MonadConfig Config m)

convProgram :: Config -> Map Integer String -> Vec.Program -> H.Module
convProgram config nameTags (Vec.Program funcs) = run $ do
    (concat -> hsDecls) <- traverse (uncurry convFunc) (itoList funcs)
    let hsExts = extensions ["MultiWayIf", "ScopedTypeVariables"]
        hsMod = H.Module H.noLoc H.main_mod hsExts Nothing Nothing [] hsDecls
    pure hsMod
  where
    extensions names = [H.LanguagePragma H.noLoc (map H.Ident names)]
    run = (`evalState` fmap Left nameTags)
        . (`runConfigT` config)
        . (`runReaderT` M.keysSet funcs)

keywords :: [String]
keywords = words
    " _ as case class data default deriving do else hiding \
    \   if import in infix infixl infixr instance let main \
    \   module newtype of qualified then type where        "

nameSafe :: String -> String
nameSafe = kwClash . startsNice . filter (liftA2 (||) isAlphaNum (=='_')) where
    startsNice (c:cs) | isLetter c = c:cs
    startsNice cs = "_" ++ cs
    kwClash cs | cs `elem` keywords = "_" ++ cs
               | otherwise = cs

resolveConflict :: H.Name -> Set H.Name -> H.Name
resolveConflict name names
    | name `S.notMember` names = name
    | otherwise = resolveConflict (change name) names
  where
    change (H.Ident str)  = H.Ident (str ++ "'")
    change _ = error "impossible happened: H.Ident expected"

nameGen :: T m => Integer -> m H.Name
nameGen n = gets (M.lookup n) >>= \case
    Nothing -> do
        modify $ M.insert n (Left (show n))
        nameGen n
    Just (Left str) -> do
        let name = H.Ident (nameSafe str)
        names <- do
            dict <- get
            let assocName (Vec.NameGen n)
                  | Just (Right name) <- M.lookup n dict
                  = S.insert name
                assocName _ = id
            asks $ S.foldr assocName S.empty
        modify $ M.insert n (Right $ resolveConflict name names)
        nameGen n
    Just (Right name) -> pure name

convType :: Vec.Type -> H.Type
convType = \case
    Vec.TypeInteger -> H.TyCon (HsIdent "Prelude" "Int")
    Vec.TypeDouble  -> H.TyCon (HsIdent "Prelude" "Double")
    Vec.TypeBoolean -> H.TyCon (HsIdent "Prelude" "Bool")
    Vec.TypeChar    -> H.TyCon (HsIdent "Prelude" "Char")
    Vec.TypeTaint   -> H.TyCon (HsIdent "Prelude" "IO")
    Vec.TypeUnit    -> H.TyCon (H.Special H.UnitCon)
    Vec.TypePair    -> H.TyCon (H.Special (H.TupleCon H.Boxed 2))
    Vec.TypeFunction-> H.TyCon (H.Special H.FunCon)
    Vec.TypeList    -> H.TyCon (H.Special H.ListCon)
    Vec.TypeBeta t1 t2 -> H.TyApp (convType t1) (convType t2)
    _ -> error "convType: unknown type"


convExpression :: (T m, C m) => Vec.Expression -> m H.Exp
convExpression = \case
    Vec.Lambda pat act
         -> H.Lambda H.noLoc
        <$> traverse convPattern [pat]
        <*> local (Vec.patBound pat <>) (convExpression act)
    Vec.Beta a1 a2 -> H.App <$> convExpression a1 <*> convExpression a2
    Vec.Primary lit -> pure $ convLit lit
    Vec.Access name -> case M.lookup name VecOp.operators of
        Just op -> pure (VecOp.hs op)
        Nothing -> case name of
            Vec.NameGen n -> nameGen n <&> \hsName -> H.Var (H.UnQual hsName)
            _ -> error "convExpression: unsupported special name"

convFunc :: (T m, C m) => Vec.Name -> Vec.Func -> m [H.Decl]
convFunc name (Vec.Func ty expression) = do
    hsName <- case name of
        Vec.NameSpecial Vec.OpMain -> pure H.main_name
        Vec.NameGen n -> nameGen n
        _ -> error "convFunc: incorrect name"
    let sig = H.TypeSig H.noLoc [hsName] (convType ty)
    hsRhs <- H.UnGuardedRhs <$> convExpression expression
    let funBind = H.FunBind [H.Match H.noLoc hsName [] Nothing hsRhs (H.BDecls [])]
    pure [sig, funBind]

convPattern :: (T m, C m) => Vec.Pattern -> m H.Pat
convPattern = \case
    Vec.PUnit -> pure $ H.PTuple H.Boxed []
    Vec.PWildCard -> pure $ H.PWildCard
    Vec.PAccess name ty -> do
        hsName <- case name of
            Vec.NameGen n -> nameGen n
            _ -> error "convPattern: incorrect name"
        configPatSig <- configPatSig <$> config
        let annotate
             | configPatSig = \pat -> H.PatTypeSig H.noLoc pat (convType ty)
             | otherwise = id
        pure $ annotate (H.PVar hsName)
    Vec.PTuple pat1 pat2 -> H.PTuple H.Boxed <$> traverse convPattern [pat1, pat2]

convLit :: Vec.Literal -> H.Exp
convLit = \case
    Vec.LitInteger n -> (if n < 0 then H.Paren else id) $ H.Lit (H.Int  n)
    Vec.LitDouble  x -> (if x < 0 then H.Paren else id) $ H.Lit (H.Frac x)
    Vec.LitChar    c -> H.Lit $ H.Char c
