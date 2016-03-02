{-# LANGUAGE LambdaCase
           , ViewPatterns
           , ScopedTypeVariables
           #-}
module Language.Haskell.Tools.AST.FromGHC.Types where
 
import SrcLoc as GHC
import RdrName as GHC
import HsTypes as GHC
import ApiAnnotation as GHC
import FastString as GHC

import Control.Monad.Reader.Class
import Control.Lens
import Data.Maybe

import Language.Haskell.Tools.AST.FromGHC.Base
import Language.Haskell.Tools.AST.FromGHC.TH
import Language.Haskell.Tools.AST.FromGHC.Kinds
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils

import Language.Haskell.Tools.AST
import qualified Language.Haskell.Tools.AST.Types as AST

trfType :: TransformName n r => Located (HsType n) -> Trf (Ann AST.Type r)
trfType = trfLoc trfType'

trfType' :: TransformName n r => HsType n -> Trf (AST.Type r)
trfType' (HsForAllTy Implicit _ _ (unLoc -> []) typ) = trfType' (unLoc typ)
trfType' (HsForAllTy Implicit _ _ ctx typ) = AST.TyCtx <$> (fromJust . view annMaybe <$> trfCtx ctx) 
                                                       <*> trfType typ
trfType' (HsForAllTy _ _ bndrs ctx typ) = AST.TyForall <$> trfBindings (hsq_tvs bndrs) 
                                                       <*> trfCtx ctx
                                                       <*> trfType typ
trfType' (HsTyVar name) = AST.TyVar <$> annCont (trfName' name)
trfType' (HsAppTy t1 t2) = AST.TyApp <$> trfType t1 <*> trfType t2
trfType' (HsFunTy t1 t2) = AST.TyFun <$> trfType t1 <*> trfType t2
trfType' (HsListTy typ) = AST.TyList <$> trfType typ
trfType' (HsPArrTy typ) = AST.TyParArray <$> trfType typ
trfType' (HsTupleTy HsBoxedTuple typs) = AST.TyTuple <$> trfAnnList trfType' typs
trfType' (HsTupleTy HsUnboxedTuple typs) = AST.TyUnbTuple <$> trfAnnList trfType' typs
trfType' (HsOpTy t1 op t2) = AST.TyInfix <$> trfType t1 <*> trfName (snd op) <*> trfType t2
trfType' (HsParTy typ) = AST.TyParen <$> trfType typ
trfType' (HsKindSig typ kind) = AST.TyKinded <$> trfType typ <*> trfKind kind
trfType' (HsQuasiQuoteTy qq) = AST.TyQuasiQuote <$> trfQuasiQuotation' qq
trfType' (HsSpliceTy splice _) = AST.TySplice <$> trfSplice' splice
trfType' (HsBangTy _ typ) = AST.TyBang <$> trfType typ
-- HsRecTy
trfType' (HsTyLit (HsNumTy _ int)) = pure $ AST.TyNumLit int
trfType' (HsTyLit (HsStrTy _ str)) = pure $ AST.TyStrLit (unpackFS str)
trfType' (HsWrapTy _ typ) = trfType' typ
trfType' HsWildcardTy = pure AST.TyWildcard
-- not implemented as ghc 7.10.3
trfType' (HsNamedWildcardTy name) = AST.TyNamedWildc <$> annCont (trfName' name)
  
trfBindings :: TransformName n r => [Located (HsTyVarBndr n)] -> Trf (AnnList AST.TyVar r)
trfBindings vars = trfAnnList trfTyVar' vars
  
trfTyVar :: TransformName n r => Located (HsTyVarBndr n) -> Trf (Ann AST.TyVar r)
trfTyVar = trfLoc trfTyVar' 
  
trfTyVar' :: TransformName n r => HsTyVarBndr n -> Trf (AST.TyVar r)
trfTyVar' (UserTyVar name) = AST.TyVarDecl <$> annLoc (asks contRange) (trfName' name) 
                                           <*> (annNothing <$> contRangeAnnot)
trfTyVar' (KindedTyVar name kind) = AST.TyVarDecl <$> trfName name <*> trfKindSig (Just kind)
  
trfCtx :: TransformName n r => Located (HsContext n) -> Trf (AnnMaybe AST.Context r)
trfCtx (L l []) = annNothing <$> contRangeAnnot
trfCtx (L l [L _ (HsParTy t)]) 
  = annJust <$> annLoc (combineSrcSpans l <$> tokenLoc AnnDarrow) 
                       (AST.ContextMulti <$> trfAnnList trfAssertion' [t])
trfCtx (L l [t]) 
  = annJust <$> annLoc (combineSrcSpans l <$> tokenLoc AnnDarrow) 
                       (AST.ContextOne <$> trfAssertion t)
trfCtx (L l ctx) = annJust <$> annLoc (combineSrcSpans l <$> tokenLoc AnnDarrow) 
                                      (AST.ContextMulti <$> trfAnnList trfAssertion' ctx) 
  
trfAssertion :: TransformName n r => LHsType n -> Trf (Ann AST.Assertion r)
trfAssertion = trfLoc trfAssertion'

trfAssertion' :: forall n r . TransformName n r => HsType n -> Trf (AST.Assertion r)
trfAssertion' t = case base of
    HsTyVar name -> AST.ClassAssert <$> annLoc (asks contRange) (trfName' name) 
                                    <*> trfAnnList trfType' args
    HsOpTy left op right -> AST.InfixAssert <$> trfType left <*> trfName (snd op) <*> trfType right
  where (args, base) = getArgs t
        getArgs :: HsType n -> ([LHsType n], HsType n)
        getArgs (HsAppTy (L _ ft) at) = case getArgs ft of (args, base) -> (args++[at], base)
        getArgs t = ([], t)
  