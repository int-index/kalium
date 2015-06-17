{-# LANGUAGE TemplateHaskell #-}
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

import Control.Ether.TH (ethereal)
import Control.Ether.Abbr
import qualified Control.Monad.Ether as Ether
import qualified Control.Monad.Ether.Implicit as I

ethereal "Scope" "scope"
ethereal "ReifiedNames" "reifiedNames"

convert :: Config -> Map Integer String -> Vec.Program -> H.Module
convert = convProgram

data Config = Config
    { configPatSig :: Bool
    } deriving (Eq)

type T m =
    ( Applicative m
    , Ether '[ Scope --> Set Vec.Name
             , ReifiedNames <-> Map Integer (Either String H.Name)
             ] m )
type C m = (Applicative m, I.MonadReader Config m)

convProgram :: Config -> Map Integer String -> Vec.Program -> H.Module
convProgram config nameTags (Vec.Program funcs) = run $ do
    (concat -> hsDecls) <- traverse (uncurry convFunc) (itoList funcs)
    let hsExts = extensions ["MultiWayIf", "ScopedTypeVariables"]
        hsMod = H.Module H.noLoc H.main_mod hsExts Nothing Nothing [] hsDecls
    pure hsMod
  where
    extensions names = [H.LanguagePragma H.noLoc (map H.Ident names)]
    run = (\m -> Ether.evalState reifiedNames m $ fmap Left nameTags)
        . (\m -> Ether.runReaderT scope m $ M.keysSet funcs)
        . (\m -> I.runReaderT m config)

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
nameGen n = Ether.gets reifiedNames (M.lookup n) >>= \case
    Nothing -> do
        Ether.modify reifiedNames $ M.insert n (Left (show n))
        nameGen n
    Just (Left str) -> do
        let name = H.Ident (nameSafe str)
        names <- do
            dict <- Ether.get reifiedNames
            let assocName (Vec.NameGen n)
                  | Just (Right name) <- M.lookup n dict
                  = S.insert name
                assocName _ = id
            Ether.asks scope (S.foldr assocName S.empty)
        Ether.modify reifiedNames $ M.insert n (Right $ resolveConflict name names)
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
        <*> Ether.local scope (Vec.patBound pat <>) (convExpression act)
    Vec.Beta a1 a2 -> H.App <$> convExpression a1 <*> convExpression a2
    Vec.Primary lit -> pure $ convLit lit
    Vec.Access name -> case M.lookup name VecOp.operators of
        Just op -> pure (VecOp.hs op)
        Nothing -> case name of
            Vec.NameGen n -> nameGen n <&> \hsName -> H.Var (H.UnQual hsName)
            _ -> error "convExpression: unsupported special name"
    Vec.Ext ext -> absurd ext

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
        configPatSig <- configPatSig <$> I.ask
        let annotate
             | configPatSig = \pat -> H.PatTypeSig H.noLoc pat (convType ty)
             | otherwise = id
        pure $ annotate (H.PVar hsName)
    Vec.PTuple pat1 pat2 -> H.PTuple H.Boxed <$> traverse convPattern [pat1, pat2]
    Vec.PExt pext -> absurd pext

convLit :: Vec.Literal -> H.Exp
convLit = \case
    Vec.LitInteger n -> (if n < 0 then H.Paren else id) $ H.Lit (H.Int  n)
    Vec.LitDouble  x -> (if x < 0 then H.Paren else id) $ H.Lit (H.Frac x)
    Vec.LitChar    c -> H.Lit $ H.Char c
