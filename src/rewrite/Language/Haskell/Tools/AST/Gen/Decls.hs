-- | Generation of declaration-level AST fragments for refactorings.
-- The bindings defined here create a the annotated version of the AST constructor with the same name.
-- For example, @mkTypeSignature@ creates the annotated version of the @UTypeSignature@ AST constructor.
{-# LANGUAGE OverloadedStrings
           , TypeFamilies
           #-}
module Language.Haskell.Tools.AST.Gen.Decls where

import qualified Name as GHC
import Data.List
import Data.String
import Data.Function (on)
import Control.Reference
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.ElementTypes
import Language.Haskell.Tools.AST.Gen.Utils
import Language.Haskell.Tools.AST.Gen.Names
import Language.Haskell.Tools.Transform

-- | Creates a type synonym ( @type String = [Char]@ )
mkTypeDecl :: DeclHead dom -> Type dom -> Decl dom 
mkTypeDecl dh typ = mkAnn (child <> " :: " <> child) $ UTypeDecl dh typ

-- | Creates a standalone deriving declaration (@ deriving instance X T @)
mkStandaloneDeriving :: Maybe (OverlapPragma dom) -> InstanceRule dom -> Decl dom
mkStandaloneDeriving overlap instRule = mkAnn ("deriving instance" <> child <> child) 
                                          $ UDerivDecl (mkAnnMaybe (optBefore " ") overlap) instRule

-- | Creates a fixity declaration (@ infixl 5 +, - @)
mkFixityDecl :: FixitySignature dom -> Decl dom
mkFixityDecl = mkAnn child . UFixityDecl

-- | Creates default types (@ default (T1, T2) @)
mkDefaultDecl :: [Type dom] -> Decl dom
mkDefaultDecl = mkAnn ("default (" <> child <> ")") . UDefaultDecl . mkAnnList (listSep ", ")

-- | Creates type signature declaration (@ f :: Int -> Int @)
mkTypeSigDecl :: TypeSignature dom -> Decl dom
mkTypeSigDecl = mkAnn child . UTypeSigDecl

-- | Creates a function or value binding (@ f x = 12 @)
mkValueBinding :: ValueBind dom -> Decl dom
mkValueBinding = mkAnn child . UValueBinding

-- | Creates a Template Haskell splice declaration (@ $(generateDecls) @)
mkSpliceDecl :: Splice dom -> Decl dom
mkSpliceDecl = mkAnn child . USpliceDecl

-- * Data type definitions

-- | Creates a data or newtype declaration.
mkDataDecl :: DataOrNewtypeKeyword dom -> Maybe (Context dom) -> DeclHead dom -> [ConDecl dom] -> Maybe (Deriving dom) -> Decl dom
mkDataDecl keyw ctx dh cons derivs 
  = mkAnn (child <> " " <> child <> child <> child <> child) 
      $ UDataDecl keyw (mkAnnMaybe (optBefore " ") ctx) dh 
                 (mkAnnList (listSepBefore " | " " = ") cons) (mkAnnMaybe (optBefore " deriving ") derivs)

-- | Creates a GADT-style data or newtype declaration.
mkGADTDataDecl :: DataOrNewtypeKeyword dom -> Maybe (Context dom) -> DeclHead dom -> Maybe (KindConstraint dom)
                    -> [GadtConDecl dom] -> Maybe (Deriving dom) -> Decl dom
mkGADTDataDecl keyw ctx dh kind cons derivs 
  = mkAnn (child <> " " <> child <> child <> child <> child <> child) 
      $ UGDataDecl keyw (mkAnnMaybe (optBefore " ") ctx) dh 
                  (mkAnnMaybe (optBefore " ") kind) (mkAnnList (listSepBefore " | " " = ") cons) (mkAnnMaybe (optBefore " deriving ") derivs)

-- | Creates a GADT constructor declaration (@ D1 :: Int -> T String @)
mkGadtConDecl :: [Name dom] -> Type dom -> GadtConDecl dom
mkGadtConDecl names typ = mkAnn (child <> " :: " <> child) $ UGadtConDecl (mkAnnList (listSep ", ") names) (mkAnn child $ UGadtNormalType typ)

-- | Creates a GADT constructor declaration with record syntax (@ D1 :: { val :: Int } -> T String @)
mkGadtRecordConDecl :: [Name dom] -> [FieldDecl dom] -> Type dom -> GadtConDecl dom
mkGadtRecordConDecl names flds typ 
  = mkAnn (child <> " :: " <> child) $ UGadtConDecl (mkAnnList (listSep ", ") names) 
      $ mkAnn (child <> " -> " <> child) $ UGadtRecordType (mkAnnList (listSepBeforeAfter ", " "{ " " }") flds) typ

-- | Creates an ordinary data constructor (@ C t1 t2 @)
mkConDecl :: Name dom -> [Type dom] -> ConDecl dom
mkConDecl name args = mkAnn (child <> child) $ UConDecl name (mkAnnList (listSepBefore " " " ") args)

-- | Creates a record data constructor (@ Point { x :: Double, y :: Double } @)
mkRecordConDecl :: Name dom -> [FieldDecl dom] -> ConDecl dom
mkRecordConDecl name fields = mkAnn (child <> " { " <> child <> " }") $ URecordDecl name (mkAnnList (listSep ", ") fields)

-- | Creates an infix data constructor (@ t1 :+: t2 @)
mkInfixConDecl :: Type dom -> Operator dom -> Type dom -> ConDecl dom
mkInfixConDecl lhs op rhs = mkAnn (child <> " " <> child <> " " <> child) $ UInfixConDecl lhs op rhs

-- | Creates a field declaration (@ fld :: Int @) for a constructor
mkFieldDecl :: [Name dom] -> Type dom -> FieldDecl dom
mkFieldDecl names typ = mkAnn (child <> " :: " <> child) $ UFieldDecl (mkAnnList (listSep ", ") names) typ

-- | Creates a deriving clause following a data type declaration. (@ deriving Show @ or @ deriving (Show, Eq) @)
mkDeriving :: [InstanceHead dom] -> Deriving dom
mkDeriving [deriv] = mkAnn child $ UDerivingOne deriv
mkDeriving derivs = mkAnn ("(" <> child <> ")") $ UDerivings (mkAnnList (listSep ", ") derivs)

-- | The @data@ keyword in a type definition
mkDataKeyword :: DataOrNewtypeKeyword dom
mkDataKeyword = mkAnn "data" UDataKeyword

-- | The @newtype@ keyword in a type definition
mkNewtypeKeyword :: DataOrNewtypeKeyword dom
mkNewtypeKeyword = mkAnn "newtype" UNewtypeKeyword

-- * Class declarations

-- | Creates a type class declaration (@ class X a where f = ... @)
mkClassDecl :: Maybe (Context dom) -> DeclHead dom -> [FunDep dom] -> Maybe (ClassBody dom) -> Decl dom
mkClassDecl ctx dh funDeps body 
  = let fdeps = case funDeps of [] -> Nothing
                                _ -> Just $ mkAnn child $ UFunDeps $ mkAnnList (listSep ", ") funDeps
     in mkAnn ("class " <> child <> child <> child <> child) 
          $ UClassDecl (mkAnnMaybe (optAfter " ") ctx) dh (mkAnnMaybe (optBefore " | ") fdeps) (mkAnnMaybe opt body) 

-- | Creates the list of declarations that can appear in a typeclass
mkClassBody :: [ClassElement dom] -> ClassBody dom
mkClassBody = mkAnn (" where " <> child) . UClassBody . mkAnnList indentedList

-- | Creates a type signature as class element: @ f :: A -> B @
mkClassElemSig :: TypeSignature dom -> ClassElement dom
mkClassElemSig = mkAnn child . UClsSig

-- | Creates a default binding as class element: @ f x = "aaa" @
mkClassElemDef :: ValueBind dom -> ClassElement dom
mkClassElemDef = mkAnn child . UClsDef

-- | Creates an associated type synonym in class: @ type T y :: * @ 
mkClassElemTypeFam :: DeclHead dom -> Maybe (TypeFamilySpec dom) -> ClassElement dom
mkClassElemTypeFam dh tfSpec = mkAnn ("type " <> child) $ UClsTypeFam (mkAnn (child <> child) $ UTypeFamily dh (mkAnnMaybe opt tfSpec))

-- | Creates an associated data synonym in class: @ data T y :: * @ 
mkClassElemDataFam :: DeclHead dom -> Maybe (KindConstraint dom) -> ClassElement dom
mkClassElemDataFam dh kind = mkAnn ("data " <> child) $ UClsTypeFam (mkAnn (child <> child) $ UDataFamily dh (mkAnnMaybe opt kind))

-- | Creates a default choice for type synonym in class: @ type T x = TE @ or @ type instance T x = TE @
mkClsDefaultType :: DeclHead dom -> Type dom -> ClassElement dom
mkClsDefaultType dh typ = mkAnn ("type " <> child <> " = " <> child) $ UClsTypeDef dh typ

-- | Creates a default signature (by using @DefaultSignatures@) in class: @ default enum :: (Generic a, GEnum (Rep a)) => [a] @
mkClsDefaultSig :: Name dom -> Type dom -> ClassElement dom
mkClsDefaultSig dh typ = mkAnn ("default " <> child <> " :: " <> child) $ UClsDefSig dh typ

-- | Creates a functional dependency, given on the form @l1 ... ln -> r1 ... rn@
mkFunDep :: [Name dom] -> [Name dom] -> FunDep dom
mkFunDep lhss rhss = mkAnn (child <> " -> " <> child) 
                       $ UFunDep (mkAnnList (listSep ", ") lhss) (mkAnnList (listSep ", ") rhss)

-- | Minimal pragma: @ {-\# MINIMAL (==) | (/=) \#-} @ in a class
mkClsMinimal :: MinimalFormula dom -> ClassElement dom
mkClsMinimal = mkAnn ("{-# MINIMAL " <> child <> " #-}") . UClsMinimal

mkMinimalName :: Name dom -> MinimalFormula dom
mkMinimalName = mkAnn child . UMinimalName

mkMinimalParen :: MinimalFormula dom -> MinimalFormula dom
mkMinimalParen = mkAnn ("(" <> child <> ")") . UMinimalParen

-- | One of the minimal formulas are needed (@ min1 | min2 @)
mkMinimalOr :: [MinimalFormula dom] -> MinimalFormula dom
mkMinimalOr = mkAnn child . UMinimalOr . mkAnnList (listSep " | ")

-- | Both of the minimal formulas are needed (@ min1 , min2 @)
mkMinimalAnd :: [MinimalFormula dom] -> MinimalFormula dom
mkMinimalAnd = mkAnn child . UMinimalAnd . mkAnnList (listSep ", ")

-- * Declaration heads

-- | Type or class name as a declaration head
mkNameDeclHead :: Name dom -> DeclHead dom
mkNameDeclHead = mkAnn child . UDeclHead

-- | Parenthesized type as a declaration head
mkParenDeclHead :: DeclHead dom -> DeclHead dom
mkParenDeclHead = mkAnn child . UDHParen

-- | Application in a declaration head
mkDeclHeadApp :: DeclHead dom -> TyVar dom -> DeclHead dom
mkDeclHeadApp dh tv = mkAnn (child <> " " <> child) $ UDHApp dh tv

-- | Infix application of the type/class name to the left operand in a declaration head
mkInfixDeclHead :: TyVar dom -> Operator dom -> TyVar dom -> DeclHead dom
mkInfixDeclHead lhs op rhs = mkAnn (child <> " " <> child <> " " <> child) $ UDHInfix lhs op rhs

-- * Type class instance declarations

-- | Creates a type class instance declaration (@ instance X T [where f = ...] @)
mkInstanceDecl :: Maybe (OverlapPragma dom) -> InstanceRule dom -> Maybe (InstBody dom) -> Decl dom
mkInstanceDecl overlap instRule body = mkAnn ("instance " <> child <> child <> child) 
                                 $ UInstDecl (mkAnnMaybe (optBefore " ") overlap) instRule (mkAnnMaybe opt body)

-- | The instance declaration rule, which is, roughly, the part of the instance declaration before the where keyword.
mkInstanceRule :: Maybe (Context dom) -> InstanceHead dom -> InstanceRule dom
mkInstanceRule ctx ih 
  = mkAnn (child <> child <> child) $ UInstanceRule (mkAnnMaybe (optBefore " ") Nothing) (mkAnnMaybe (optBefore " ") ctx) ih

-- | Type or class name as a part of the instance declaration
mkInstanceHead :: Name dom -> InstanceHead dom
mkInstanceHead = mkAnn child . UInstanceHeadCon

-- | Infix application of the type/class name to the left operand as a part of the instance declaration
mkInfixInstanceHead :: Type dom -> Name dom -> InstanceHead dom
mkInfixInstanceHead typ n = mkAnn (child <> child) $ UInstanceHeadInfix typ n

-- | Parenthesized instance head as a part of the instance declaration
mkParenInstanceHead :: InstanceHead dom -> InstanceHead dom
mkParenInstanceHead = mkAnn ("(" <> child <> ")") . UInstanceHeadParen

-- | Application to one more type as a part of the instance declaration
mkAppInstanceHead :: InstanceHead dom -> Type dom -> InstanceHead dom
mkAppInstanceHead fun arg = mkAnn (child <> " " <> child) $ UInstanceHeadApp fun arg

-- | Instance body is the implementation of the class functions (@ where a x = 1; b x = 2 @)
mkInstanceBody :: [InstBodyDecl dom] -> InstBody dom
mkInstanceBody = mkAnn (" where " <> child) . UInstBody . mkAnnList indentedList

-- | A normal declaration (@ f x = 12 @) in a type class instance
mkInstanceBind :: ValueBind dom -> InstBodyDecl dom
mkInstanceBind = mkAnn child . UInstBodyNormalDecl

-- | Type signature in instance definition with @InstanceSigs@
mkInstanceTypeSig :: TypeSignature dom -> InstBodyDecl dom
mkInstanceTypeSig = mkAnn child . UInstBodyTypeSig

-- | An associated type definition (@ type A X = B @) in a type class instance
mkInstanceTypeFamilyDef :: TypeEqn dom -> InstBodyDecl dom
mkInstanceTypeFamilyDef = mkAnn child . UInstBodyTypeDecl

-- | An associated data type implementation (@ data A X = C1 | C2 @) int a type class instance
mkInstanceDataFamilyDef :: DataOrNewtypeKeyword dom -> InstanceRule dom -> [ConDecl dom] -> Maybe (Deriving dom) -> InstBodyDecl dom
mkInstanceDataFamilyDef keyw instRule cons derivs 
  = mkAnn (child <> " " <> child <> child <> child) 
      $ UInstBodyDataDecl keyw instRule (mkAnnList (listSepBefore " | " " = ") cons) (mkAnnMaybe (optBefore " deriving ") derivs)

-- | An associated data type implemented using GADT style int a type class instance
mkInstanceDataFamilyGADTDef :: DataOrNewtypeKeyword dom -> InstanceRule dom -> Maybe (KindConstraint dom) -> [GadtConDecl dom] 
                                 -> Maybe (Deriving dom) -> InstBodyDecl dom
mkInstanceDataFamilyGADTDef keyw instRule kind cons derivs 
  = mkAnn (child <> " " <> child <> child <> child) 
      $ UInstBodyGadtDataDecl mkDataKeyword instRule (mkAnnMaybe opt kind) (mkAnnList (listSepBefore " | " " = ") cons) 
                             (mkAnnMaybe (optBefore " deriving ") derivs)

-- | Specialize instance pragma (no phase selection is allowed) in a type class instance
mkInstanceSpecializePragma :: Type dom -> InstBodyDecl dom
mkInstanceSpecializePragma = mkAnn ("{-# SPECIALIZE " <> child <> " #-}") . USpecializeInstance

-- | @OVERLAP@ pragma for type instance definitions
mkEnableOverlap :: OverlapPragma dom     
mkEnableOverlap = mkAnn "{-# OVERLAP #-}" UEnableOverlap

-- | @NO_OVERLAP@ pragma for type instance definitions
mkDisableOverlap :: OverlapPragma dom    
mkDisableOverlap = mkAnn "{-# NO_OVERLAP #-}" UDisableOverlap

-- | @OVERLAPPABLE@ pragma for type instance definitions
mkOverlappable :: OverlapPragma dom      
mkOverlappable = mkAnn "{-# OVERLAPPABLE #-}" UOverlappable

-- | @OVERLAPPING@ pragma for type instance definitions
mkOverlapping :: OverlapPragma dom       
mkOverlapping = mkAnn "{-# OVERLAPPING #-}" UOverlapping

-- | @OVERLAPS@ pragma for type instance definitions
mkOverlaps :: OverlapPragma dom          
mkOverlaps = mkAnn "{-# OVERLAPS #-}" UOverlaps

-- | @INCOHERENT@ pragma for type instance definitions
mkIncoherentOverlap :: OverlapPragma dom 
mkIncoherentOverlap = mkAnn "{-# INCOHERENT #-}" UIncoherentOverlap

-- * Type roles

-- | Creates a role annotations (@ type role Ptr representational @)
mkRoleDecl :: QualifiedName dom -> [Role dom] -> Decl dom
mkRoleDecl name roles = mkAnn ("type role " <> child <> child) $ URoleDecl name $ mkAnnList (listSepBefore " " " ") roles

-- | Marks a given type parameter as @nominal@.
mkNominalRole :: Role dom
mkNominalRole = mkAnn "nominal" UNominal 

-- | Marks a given type parameter as @representational@.
mkRepresentationalRole :: Role dom
mkRepresentationalRole = mkAnn "representational" URepresentational

-- | Marks a given type parameter as @phantom@.
mkPhantomRole :: Role dom
mkPhantomRole = mkAnn "phantom" UPhantom

-- * Foreign imports and exports

-- | Creates a foreign import (@ foreign import foo :: Int -> IO Int @)
mkForeignImport :: CallConv dom -> Maybe (Safety dom) -> Name dom -> Type dom -> Decl dom
mkForeignImport cc safety name typ = mkAnn (child <> child <> " " <> child <> " :: " <> child) 
                                       $ UForeignImport cc (mkAnnMaybe (optBefore " ") safety) name typ

-- | Creates a foreign export (@ foreign export ccall foo :: Int -> IO Int @)
mkForeignExport :: CallConv dom -> Name dom -> Type dom -> Decl dom
mkForeignExport cc name typ = mkAnn (child <> " " <> child <> " :: " <> child) $ UForeignExport cc name typ

-- | Specifies @stdcall@ calling convention for foreign import/export.
mkStdCall :: CallConv dom
mkStdCall = mkAnn "stdcall" UStdCall

-- | Specifies @ccall@ calling convention for foreign import/export.
mkCCall :: CallConv dom
mkCCall = mkAnn "ccall" UCCall

-- | Specifies @capi@ calling convention for foreign import/export.
mkCApi :: CallConv dom
mkCApi = mkAnn "capi" UCApi

-- | Specifies that the given foreign import is @unsafe@.
mkUnsafe :: Safety dom
mkUnsafe = mkAnn "unsafe" UUnsafe 

-- * Type and data families

-- | Creates a type family declaration ( @type family F x@ )
mkTypeFamily :: DeclHead dom -> Maybe (TypeFamilySpec dom) -> Decl dom
mkTypeFamily dh famSpec = mkAnn child $ UTypeFamilyDecl (mkAnn (child <> child) $ UTypeFamily dh (mkAnnMaybe (optBefore " ") famSpec))

-- | Creates a closed type family declaration ( @type family F x where F Int = (); F a = Int@ )
mkClosedTypeFamily :: DeclHead dom -> Maybe (KindConstraint dom) -> [TypeEqn dom] -> Decl dom
mkClosedTypeFamily dh kind typeqs = mkAnn (child <> child <> " where " <> child) 
                                      $ UClosedTypeFamilyDecl dh (mkAnnMaybe (optBefore " ") kind) (mkAnnList indentedList typeqs)

-- | Creates a data family declaration (@ data family A a :: * -> * @)   
mkDataFamily :: DeclHead dom -> Maybe (KindConstraint dom) -> Decl dom
mkDataFamily dh kind = mkAnn child $ UTypeFamilyDecl (mkAnn (child <> child) $ UDataFamily dh (mkAnnMaybe (optBefore " ") kind))

-- | Specifies the kind of a type family (@ :: * -> * @)
mkTypeFamilyKindSpec :: KindConstraint dom -> TypeFamilySpec dom
mkTypeFamilyKindSpec = mkAnn child . UTypeFamilyKind

-- | Specifies the injectivity of a type family (@ = r | r -> a @)
mkTypeFamilyInjectivitySpec :: Name dom -> [Name dom] -> TypeFamilySpec dom
mkTypeFamilyInjectivitySpec res dependent 
  = mkAnn child (UTypeFamilyInjectivity $ mkAnn (child <> " -> " <> child) $ UInjectivityAnn res (mkAnnList (listSep " ") dependent))

-- | Type equations as found in closed type families (@ T A = S @)
mkTypeEqn :: Type dom -> Type dom -> TypeEqn dom
mkTypeEqn lhs rhs = mkAnn (child <> " = " <> child) $ UTypeEqn lhs rhs

-- | Creates a type family instance declaration (@ type instance Fam T = AssignedT @)
mkTypeInstance :: InstanceRule dom -> Type dom -> Decl dom
mkTypeInstance instRule typ = mkAnn ("type instance " <> child <> " = " <> child) $ UTypeInstDecl instRule typ

-- | Creates a data instance declaration (@ data instance Fam T = Con1 | Con2 @)
mkDataInstance :: DataOrNewtypeKeyword dom -> InstanceRule dom -> [ConDecl dom] -> Maybe (Deriving dom) -> Decl dom
mkDataInstance keyw instRule cons derivs 
  = mkAnn (child <> " instance " <> child <> " = " <> child <> child) 
      $ UDataInstDecl keyw instRule (mkAnnList (listSepBefore " | " " = ") cons) (mkAnnMaybe (optBefore " deriving ") derivs)

-- | Creates a GADT-style data instance declaration (@ data instance Fam T where ... @)
mkGadtDataInstance :: DataOrNewtypeKeyword dom -> InstanceRule dom -> Maybe (KindConstraint dom) -> [GadtConDecl dom] -> Decl dom
mkGadtDataInstance keyw instRule kind cons 
  = mkAnn (child <> " instance " <> child <> child <> " where " <> child) 
      $ UGDataInstDecl keyw instRule (mkAnnMaybe (optBefore " ") kind) (mkAnnList indentedList cons)

-- * Pattern synonyms

-- | Creates a pattern synonym (@ pattern Arrow t1 t2 = App \"->\" [t1, t2] @)
mkPatternSynonym :: PatSynLhs dom -> PatSynRhs dom -> Decl dom
mkPatternSynonym lhs rhs = mkAnn child $ UPatternSynonymDecl $ mkAnn ("pattern " <> child <> " " <> child) $ UPatternSynonym lhs rhs

-- | Creates a left hand side of a pattern synonym with a constructor name and arguments (@ Arrow t1 t2 @)
mkConPatSyn :: Name dom -> [Name dom] -> PatSynLhs dom
mkConPatSyn con args = mkAnn (child <> child) $ UNormalPatSyn con $ mkAnnList (listSepBefore " " " ") args

-- | Creates an infix pattern synonym left-hand side (@ t1 :+: t2 @)
mkInfixPatSyn :: Name dom -> Operator dom -> Name dom -> PatSynLhs dom
mkInfixPatSyn lhs op rhs = mkAnn (child <> " " <> child <> " " <> child) $ UInfixPatSyn lhs op rhs

-- | Creates a record-style pattern synonym left-hand side (@ Arrow { arrowFrom, arrowTo } @)
mkRecordPatSyn :: Name dom -> [Name dom] -> PatSynLhs dom
mkRecordPatSyn con args = mkAnn (child <> child) $ URecordPatSyn con $ mkAnnList (listSepBeforeAfter ", " "{ " " }") args

-- | Creates an automatically two-way pattern synonym (@ = App \"Int\" [] @)
mkSymmetricPatSyn :: Pattern dom -> PatSynRhs dom
mkSymmetricPatSyn = mkAnn ("= " <> child) . flip UBidirectionalPatSyn (mkAnnMaybe opt Nothing)

-- | Creates a pattern synonym that can be only used for pattenr matching but not for combining (@ <- App \"Int\" [] @)
mkOneWayPatSyn :: Pattern dom -> PatSynRhs dom
mkOneWayPatSyn = mkAnn ("<- " <> child) . UOneDirectionalPatSyn

-- | Creates a pattern synonym with the other direction explicitely specified (@ <- App \"Int\" [] where Int = App \"Int\" [] @)
mkTwoWayPatSyn :: Pattern dom -> [Match dom] -> PatSynRhs dom
mkTwoWayPatSyn pat match = mkAnn ("<- " <> child <> child) $ UBidirectionalPatSyn pat $ mkAnnMaybe (optBefore " where ") 
                             $ Just $ mkAnn child $ UPatSynWhere $ mkAnnList indentedList match

-- | Creates a pattern type signature declaration (@ pattern Succ :: Int -> Int @)
mkPatternSignatureDecl :: PatternSignature dom -> Decl dom
mkPatternSignatureDecl = mkAnn child . UPatTypeSigDecl

mkPatternSignature :: Name dom -> Type dom -> PatternSignature dom
mkPatternSignature name typ = mkAnn (child <> " :: " <> child) $ UPatternTypeSignature name typ

-- * Top level pragmas

-- | Creates a top-level pragmas
mkPragmaDecl :: TopLevelPragma dom -> Decl dom
mkPragmaDecl = mkAnn child . UPragmaDecl

-- | A pragma that introduces source rewrite rules (@ {-\# RULES "map/map" [2]  forall f g xs. map f (map g xs) = map (f.g) xs \#-} @)
mkRulePragma :: [Rule dom] -> TopLevelPragma dom
mkRulePragma = mkAnn ("{-# RULES " <> child <> " #-}") . URulePragma . mkAnnList (listSep ", ")

-- | A pragma that marks definitions as deprecated (@ {-\# DEPRECATED f "f will be replaced by g" \#-} @)
mkDeprPragma :: [Name dom] -> String -> TopLevelPragma dom
mkDeprPragma defs msg = mkAnn ("{-# DEPRECATED " <> child <> " " <> child <> " #-}") 
                          $ UDeprPragma (mkAnnList (listSep ", ") defs) $ mkAnn ("\"" <> child <> "\"") $ UStringNode msg

-- | A pragma that marks definitions as deprecated (@ {-\# WARNING unsafePerformIO "you should know what you are doing" \#-} @)
mkWarningPragma :: [Name dom] -> String -> TopLevelPragma dom
mkWarningPragma defs msg = mkAnn ("{-# WARNING " <> child <> " " <> child <> " #-}") 
                             $ UWarningPragma (mkAnnList (listSep ", ") defs) $ mkAnn ("\"" <> child <> "\"") $ UStringNode msg

-- | A pragma that annotates a definition with an arbitrary value (@ {-\# ANN f 42 \#-} @)
mkAnnPragma :: AnnotationSubject dom -> Expr dom -> TopLevelPragma dom
mkAnnPragma subj ann = mkAnn ("{-# ANN " <> child <> " " <> child <> " #-}") $ UAnnPragma subj ann

-- | A pragma that marks a function for inlining to the compiler (@ {-\# INLINE thenUs \#-} @)
mkInlinePragma :: Maybe (ConlikeAnnot dom) -> Maybe (PhaseControl dom) -> Name dom -> TopLevelPragma dom
mkInlinePragma conlike phase name 
  = mkAnn ("{-# INLINE " <> child <> child <> child <> " #-}") 
      $ UInlinePragma (mkAnnMaybe (optAfter " ") conlike) (mkAnnMaybe (optAfter " ") phase) name

-- | A pragma that forbids a function from being inlined by the compiler (@ {-\# NOINLINE f \#-} @)
mkNoInlinePragma :: Maybe (ConlikeAnnot dom) -> Maybe (PhaseControl dom) -> Name dom -> TopLevelPragma dom
mkNoInlinePragma conlike phase name 
  = mkAnn ("{-# NOINLINE " <> child <> child <> child <> " #-}") 
     $ UNoInlinePragma (mkAnnMaybe (optAfter " ") conlike) (mkAnnMaybe (optAfter " ") phase) name

-- | A pragma that marks a function that it may be inlined by the compiler (@ {-\# INLINABLE thenUs \#-} @)
mkInlinablePragma :: Maybe (PhaseControl dom) -> Name dom -> TopLevelPragma dom
mkInlinablePragma phase name
  = mkAnn ("{-# INLINEABLE " <> child <> child <> " #-}") 
     $ UInlinablePragma (mkAnnMaybe (optAfter " ") phase) name

-- | A pragma for maintaining line numbers in generated sources (@ {-\# LINE 123 "somefile" \#-} @)
mkLinePragma :: Int -> Maybe (StringNode dom) -> TopLevelPragma dom
mkLinePragma line filename 
  = mkAnn ("{-# LINE " <> child <> child <> " #-}") 
     $ ULinePragma (mkAnn child $ LineNumber line) (mkAnnMaybe (optBefore " ") filename)

-- | A pragma that tells the compiler that a polymorph function should be optimized for a given type (@ {-\# SPECIALISE f :: Int -> b -> b \#-} @)
mkSpecializePragma :: Maybe (PhaseControl dom) -> Name dom -> [Type dom] -> TopLevelPragma dom
mkSpecializePragma phase def specTypes 
  = mkAnn ("{-# SPECIALIZE " <> child <> child <> " " <> child <> " #-}") 
     $ USpecializePragma (mkAnnMaybe (optBefore " ") phase) def $ mkAnnList (listSep ", ") specTypes

-- | Marks that the pragma should be applied from a given compile phase (@ [2] @)
mkPhaseControlFrom :: Integer -> PhaseControl dom
mkPhaseControlFrom phaseNum 
  = mkAnn ("[" <> child <> child <> "]") $ UPhaseControl (mkAnnMaybe opt Nothing) (mkAnn child $ PhaseNumber phaseNum)

-- | Marks that the pragma should be applied until a given compile phase (@ [~2] @)
mkPhaseControlUntil :: Integer -> PhaseControl dom
mkPhaseControlUntil phaseNum 
  = mkAnn ("[" <> child <> child <> "]") $ UPhaseControl (mkAnnMaybe opt $ Just $ mkAnn "~" PhaseInvert) 
                                                         (mkAnn child $ PhaseNumber phaseNum)

-- | A rewrite rule (@ "map/map" forall f g xs. map f (map g xs) = map (f.g) xs @)
mkRewriteRule :: String -> Maybe (PhaseControl dom) -> [TyVar dom] -> Expr dom -> Expr dom -> Rule dom
mkRewriteRule name phase vars lhs rhs
  = mkAnn (child <> " " <> child <> child <> child <> " = " <> child)
      $ URule (mkAnn ("\"" <> child <> "\"") $ UStringNode name) (mkAnnMaybe (optAfter " ") phase)
              (mkAnnList (listSepBeforeAfter " " "forall " ". ") vars) lhs rhs

-- | The definition with the given name is annotated
mkNameAnnotation :: Name dom -> AnnotationSubject dom
mkNameAnnotation name = mkAnn child $ UNameAnnotation name

-- | A type with the given name is annotated
mkTypeAnnotation :: Name dom -> AnnotationSubject dom
mkTypeAnnotation name = mkAnn ("type " <> child) $ UTypeAnnotation name

-- | The whole module is annotated
mkModuleAnnotation :: AnnotationSubject dom
mkModuleAnnotation = mkAnn "module" UModuleAnnotation

-- | A @CONLIKE@ modifier for an @INLINE@ pragma.
mkConlikeAnnotation :: ConlikeAnnot dom
mkConlikeAnnotation = mkAnn "CONLIKE" UConlikeAnnot
