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

import Language.Haskell.Tools.AST.ClassyPlate

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
makeClassyPlate [] ''QQString
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
makeClassyPlate [] ''Number
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
makeClassyPlate [] ''ULanguageExtension
makeClassyPlate [] ''UMatchLhs

-- ULiteral
makeClassyPlate [] ''ULiteral
makeClassyPlate [] ''UPromoted

-- Base
makeClassyPlate [] ''UOperator
makeClassyPlate [] ''UName
makeClassyPlate [] ''UQualifiedName
makeClassyPlate [] ''UModuleName
makeClassyPlate [] ''UNamePart
makeClassyPlate [] ''UStringNode
makeClassyPlate [] ''UDataOrNewtypeKeyword
makeClassyPlate [] ''UDoKind
makeClassyPlate [] ''TypeKeyword
makeClassyPlate [] ''UOverlapPragma
makeClassyPlate [] ''UCallConv
makeClassyPlate [] ''UArrowAppl
makeClassyPlate [] ''USafety
makeClassyPlate [] ''UConlikeAnnot
makeClassyPlate [] ''Assoc
makeClassyPlate [] ''Precedence
makeClassyPlate [] ''LineNumber
makeClassyPlate [] ''UPhaseControl
makeClassyPlate [] ''PhaseNumber
makeClassyPlate [] ''PhaseInvert