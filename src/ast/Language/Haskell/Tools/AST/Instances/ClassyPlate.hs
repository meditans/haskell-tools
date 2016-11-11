{-# LANGUAGE TemplateHaskell
           , DataKinds
           , FlexibleInstances
           , MultiParamTypeClasses
           , FlexibleContexts
           , UndecidableInstances
           , TypeFamilies
           , ScopedTypeVariables
           #-}
module Language.Haskell.Tools.AST.Instances.ClassyPlate where

import Data.Generics.ClassyPlate

import Language.Haskell.Tools.AST.Representation.Modules
import Language.Haskell.Tools.AST.Representation.TH
import Language.Haskell.Tools.AST.Representation.Decls
import Language.Haskell.Tools.AST.Representation.Binds
import Language.Haskell.Tools.AST.Representation.Exprs
import Language.Haskell.Tools.AST.Representation.Stmts
import Language.Haskell.Tools.AST.Representation.Patterns
import Language.Haskell.Tools.AST.Representation.Types
import Language.Haskell.Tools.AST.Representation.Kinds
import Language.Haskell.Tools.AST.Representation.Literals
import Language.Haskell.Tools.AST.Representation.Names
import Language.Haskell.Tools.AST.Ann


-- this instance will invoke the app only once on a list
instance (GoodOperationFor c [a], ClassyPlate c a) => ClassyPlate c [a] where
  classyTraverse_ t f ls = app (undefined :: FlagToken (AppSelector c [a])) t f $ map (classyTraverse_ t f) ls
  classyTraverseM_ t f ls = appM (undefined :: FlagToken (AppSelector c [a])) t f =<< mapM (classyTraverseM_ t f) ls
  selectiveTraverse_ t f pred ls = appIf t f pred ls (map (selectiveTraverse_ t f pred) ls)
  selectiveTraverseM_ t f pred ls = appIfM t f pred ls (mapM (selectiveTraverseM_ t f pred) ls)

instance (GoodOperationFor c (Maybe a), ClassyPlate c a) => ClassyPlate c (Maybe a) where
  classyTraverse_ t f mb = app (undefined :: FlagToken (AppSelector c (Maybe a))) t f $ fmap (classyTraverse_ t f) mb
  classyTraverseM_ t f mb = appM (undefined :: FlagToken (AppSelector c (Maybe a))) t f =<< maybe (return Nothing) (fmap Just . classyTraverseM_ t f) mb
  selectiveTraverse_ t f pred mb = appIf t f pred mb (fmap (selectiveTraverse_ t f pred) mb)
  selectiveTraverseM_ t f pred mb = appIfM t f pred mb (maybe (return Nothing) (fmap Just . selectiveTraverseM_ t f pred) mb)

-- Annotations
makeClassyPlate [ '_annotation ] ''Ann
makeClassyPlate [ '_annMaybeAnnot ] ''AnnMaybeG
makeClassyPlate [ '_annListAnnot ] ''AnnListG

-- Modules
makeClassyPlate [] ''UModule
makeClassyPlate [] ''UModuleHead
makeClassyPlate [] ''UExportSpecs
makeClassyPlate [] ''UExportSpec
makeClassyPlate [] ''UIESpec
makeClassyPlate [] ''USubSpec
makeClassyPlate [] ''UModulePragma
makeClassyPlate [] ''UFilePragma
makeClassyPlate [] ''UImportDecl
makeClassyPlate [] ''UImportSpec
makeClassyPlate [] ''UImportQualified
makeClassyPlate [] ''UImportSource
makeClassyPlate [] ''UImportSafe
makeClassyPlate [] ''UTypeNamespace
makeClassyPlate [] ''UImportRenaming

-- Declarations
makeClassyPlate [] ''UDecl
makeClassyPlate [] ''UClassBody
makeClassyPlate [] ''UClassElement
makeClassyPlate [] ''UDeclHead
makeClassyPlate [] ''UInstBody
makeClassyPlate [] ''UInstBodyDecl
makeClassyPlate [] ''UGadtConDecl
makeClassyPlate [] ''UGadtConType
makeClassyPlate [] ''UFieldWildcard
makeClassyPlate [] ''UFunDeps
makeClassyPlate [] ''UFunDep
makeClassyPlate [] ''UConDecl
makeClassyPlate [] ''UFieldDecl
makeClassyPlate [] ''UDeriving
makeClassyPlate [] ''UInstanceRule
makeClassyPlate [] ''UInstanceHead
makeClassyPlate [] ''UTypeEqn
makeClassyPlate [] ''UKindConstraint
makeClassyPlate [] ''UTyVar
makeClassyPlate [] ''UType
makeClassyPlate [] ''UKind
makeClassyPlate [] ''UContext
makeClassyPlate [] ''UAssertion
makeClassyPlate [] ''UExpr
makeClassyPlate [] ''UStmt'
makeClassyPlate [] ''UCompStmt
makeClassyPlate [] ''UValueBind
makeClassyPlate [] ''UPattern
makeClassyPlate [] ''UPatternField
makeClassyPlate [] ''USplice
makeClassyPlate [ '_qqString ] ''QQString
makeClassyPlate [] ''UMatch
makeClassyPlate [] ''UAlt'
makeClassyPlate [] ''URhs
makeClassyPlate [] ''UGuardedRhs
makeClassyPlate [] ''UFieldUpdate
makeClassyPlate [] ''UBracket
makeClassyPlate [] ''UTopLevelPragma
makeClassyPlate [] ''URule
makeClassyPlate [] ''UAnnotationSubject
makeClassyPlate [] ''UMinimalFormula
makeClassyPlate [] ''UExprPragma
makeClassyPlate [] ''USourceRange
makeClassyPlate [ '_numberInteger ] ''Number
makeClassyPlate [] ''UQuasiQuote
makeClassyPlate [] ''URhsGuard
makeClassyPlate [] ''ULocalBind
makeClassyPlate [] ''ULocalBinds
makeClassyPlate [] ''UFixitySignature
makeClassyPlate [] ''UTypeSignature
makeClassyPlate [] ''UListCompBody
makeClassyPlate [] ''UTupSecElem
makeClassyPlate [] ''UTypeFamily
makeClassyPlate [] ''UTypeFamilySpec
makeClassyPlate [] ''UInjectivityAnn
makeClassyPlate [] ''UCaseRhs'
makeClassyPlate [] ''UGuardedCaseRhs'
makeClassyPlate [] ''UPatternSynonym
makeClassyPlate [] ''UPatSynRhs
makeClassyPlate [] ''UPatSynLhs
makeClassyPlate [] ''UPatSynWhere
makeClassyPlate [] ''UPatternTypeSignature
makeClassyPlate [] ''URole
makeClassyPlate [] ''UCmd
makeClassyPlate [ '_langExt ] ''ULanguageExtension
makeClassyPlate [] ''UMatchLhs

-- ULiteral
makeClassyPlate [ '_charLitValue, '_stringLitValue, '_intLitValue, '_fracLitValue, '_floatLitValue ] ''ULiteral
makeClassyPlate [ '_promotedIntValue, '_promotedStringValue ] ''UPromoted

-- Base
makeClassyPlate [] ''UOperator
makeClassyPlate [] ''UName
makeClassyPlate [] ''UQualifiedName
makeClassyPlate [ '_moduleNameString ] ''UModuleName
makeClassyPlate [ '_simpleNameStr ] ''UNamePart
makeClassyPlate [ '_stringNodeStr ] ''UStringNode
makeClassyPlate [] ''UDataOrNewtypeKeyword
makeClassyPlate [] ''UDoKind
makeClassyPlate [] ''TypeKeyword
makeClassyPlate [] ''UOverlapPragma
makeClassyPlate [] ''UCallConv
makeClassyPlate [] ''UArrowAppl
makeClassyPlate [] ''USafety
makeClassyPlate [] ''UConlikeAnnot
makeClassyPlate [] ''Assoc
makeClassyPlate [ '_precedenceValue ] ''Precedence
makeClassyPlate [ '_lineNumber ] ''LineNumber
makeClassyPlate [] ''UPhaseControl
makeClassyPlate [ '_phaseNum ] ''PhaseNumber
makeClassyPlate [] ''PhaseInvert