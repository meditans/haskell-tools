{-# LANGUAGE LambdaCase
           , ViewPatterns
           , ScopedTypeVariables
           , AllowAmbiguousTypes
           #-}
-- | Functions that convert the pattern-related elements of the GHC AST to corresponding elements in the Haskell-tools AST representation
module Language.Haskell.Tools.AST.FromGHC.Patterns where

import SrcLoc as GHC
import RdrName as GHC
import HsTypes as GHC
import HsPat as GHC
import HsLit as GHC
import ApiAnnotation as GHC
import BasicTypes as GHC
import Unique as GHC
import Debug.Trace
import Data.List
import Language.Haskell.Tools.AST.FromGHC.GHCUtils

import Language.Haskell.Tools.AST.FromGHC.Names
import {-# SOURCE #-} Language.Haskell.Tools.AST.FromGHC.TH
import Language.Haskell.Tools.AST.FromGHC.Literals
import Language.Haskell.Tools.AST.FromGHC.Types
import {-# SOURCE #-} Language.Haskell.Tools.AST.FromGHC.Exprs
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Utils

import Language.Haskell.Tools.AST (Ann(..), Dom, RangeStage)
import qualified Language.Haskell.Tools.AST as AST

trfPattern :: TransformName n r => Located (Pat n) -> Trf (Ann AST.UPattern (Dom r) RangeStage)
-- field wildcards are not directly represented in GHC AST
trfPattern (L l (ConPatIn name (RecCon (HsRecFields flds _)))) | any ((l ==) . getLoc) flds 
  = do let (fromWC, notWC) = partition ((l ==) . getLoc) flds
       normalFields <- mapM (trfLocNoSema trfPatternField') notWC
       wildc <- annLocNoSema (tokenLoc AnnDotdot) (AST.UFieldWildcardPattern <$> annCont (createImplicitFldInfo (unLoc . (\(VarPat n) -> n) . unLoc) (map unLoc fromWC)) (pure AST.FldWildcard))
       annLocNoSema (pure l) (AST.URecPat <$> trfName name <*> makeNonemptyList ", " (pure (normalFields ++ [wildc])))
trfPattern p | otherwise = trfLocNoSema trfPattern' (correctPatternLoc p)

-- | Locations for right-associative infix patterns are incorrect in GHC AST
correctPatternLoc :: Located (Pat n) -> Located (Pat n)
correctPatternLoc (L l p@(ConPatIn name (InfixCon left right)))
  = L (getLoc (correctPatternLoc left) `combineSrcSpans` getLoc (correctPatternLoc right)) p
correctPatternLoc p = p

trfPattern' :: TransformName n r => Pat n -> Trf (AST.UPattern (Dom r) RangeStage)
trfPattern' (WildPat _) = pure AST.UWildPat
trfPattern' (VarPat name) = define $ AST.UVarPat <$> trfName name
trfPattern' (LazyPat pat) = AST.UIrrefutablePat <$> trfPattern pat
trfPattern' (AsPat name pat) = AST.UAsPat <$> define (trfName name) <*> trfPattern pat
trfPattern' (ParPat pat) = AST.UParenPat <$> trfPattern pat
trfPattern' (BangPat pat) = AST.UBangPat <$> trfPattern pat
trfPattern' (ListPat pats _ _) = AST.UListPat <$> trfAnnList ", " trfPattern' pats
trfPattern' (TuplePat pats Boxed _) = AST.UTuplePat <$> trfAnnList ", " trfPattern' pats
trfPattern' (PArrPat pats _) = AST.UParArrPat <$> trfAnnList ", " trfPattern' pats
trfPattern' (ConPatIn name (PrefixCon args)) = AST.UAppPat <$> trfName name <*> trfAnnList " " trfPattern' args
trfPattern' (ConPatIn name (RecCon (HsRecFields flds _))) = AST.URecPat <$> trfName name <*> trfAnnList ", " trfPatternField' flds
trfPattern' (ConPatIn name (InfixCon left right)) = AST.UInfixAppPat <$> trfPattern left <*> trfOperator name <*> trfPattern right
trfPattern' (ViewPat expr pat _) = AST.UViewPat <$> trfExpr expr <*> trfPattern pat
trfPattern' (SplicePat splice) = AST.USplicePat <$> annContNoSema (trfSplice' splice)
trfPattern' (LitPat lit) = AST.ULitPat <$> annContNoSema (trfLiteral' lit)
trfPattern' (SigPatIn pat (hswc_body . hsib_body -> typ)) = AST.UTypeSigPat <$> trfPattern pat <*> trfType typ
trfPattern' (NPat (ol_val . unLoc -> lit) _ _ _) = AST.ULitPat <$> annContNoSema (trfOverloadedLit lit)
trfPattern' (NPlusKPat id (L l lit) _ _ _ _) = AST.UNPlusKPat <$> define (trfName id) <*> annLocNoSema (pure l) (trfOverloadedLit (ol_val lit))
-- coercion pattern introduced by GHC
trfPattern' (CoPat _ pat _) = trfPattern' pat

trfPatternField' :: TransformName n r => HsRecField n (LPat n) -> Trf (AST.UPatternField (Dom r) RangeStage)
trfPatternField' (HsRecField id arg False) = AST.UNormalFieldPattern <$> trfName (getFieldOccName id) <*> trfPattern arg
trfPatternField' (HsRecField id _ True) = AST.UFieldPunPattern <$> trfName (getFieldOccName id)