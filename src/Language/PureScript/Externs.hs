{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveAnyClass #-}
-- |
-- This module generates code for \"externs\" files, i.e. files containing only
-- foreign import declarations.
--
module Language.PureScript.Externs
  ( ExternsFile(..)
  , ExternsImport(..)
  , ExternsFixity(..)
  , ExternsTypeFixity(..)
  , ExternsDeclaration(..)
  , externsIsCurrentVersion
  , moduleToExternsFile
  , applyExternsFileToEnvironment
  , externsFileName
  ) where

import Prelude

import Codec.Serialise (Serialise)
import Control.Monad (join)
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.List (foldl', find)
import Data.Foldable (fold)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Version (showVersion)
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NEL

import Language.PureScript.AST
import Language.PureScript.AST.Declarations.ChainId (ChainId)
import Language.PureScript.Crash
import Language.PureScript.Environment
import Language.PureScript.Names
import Language.PureScript.TypeClassDictionaries
import Language.PureScript.Types

import Paths_purescript as Paths

import Debug.Trace
import PrettyPrint
import Control.Monad.Trans.State.Strict
import Control.Monad
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Monoid
import Data.Semigroup
import Language.PureScript.Names
import qualified Language.PureScript.Names as N
import Data.Foldable
import Language.PureScript.Roles (Role)


-- | The data which will be serialized to an externs file
data ExternsFile = ExternsFile
  -- NOTE: Make sure to keep `efVersion` as the first field in this
  -- record, so the derived Serialise instance produces CBOR that can
  -- be checked for its version independent of the remaining format
  { efVersion :: Text
  -- ^ The externs version
  , efModuleName :: ModuleName
  -- ^ Module name
  , efExports :: [DeclarationRef]
  -- ^ List of module exports
  , efImports :: [ExternsImport]
  -- ^ List of module imports
  , efFixities :: [ExternsFixity]
  -- ^ List of operators and their fixities
  , efTypeFixities :: [ExternsTypeFixity]
  -- ^ List of type operators and their fixities
  , efDeclarations :: [ExternsDeclaration]
  -- ^ List of type and value declaration
  , efSourceSpan :: SourceSpan
  -- ^ Source span for error reporting
  } deriving (Show, Generic)

instance Serialise ExternsFile

-- | A module import in an externs file
data ExternsImport = ExternsImport
  {
  -- | The imported module
    eiModule :: ModuleName
  -- | The import type: regular, qualified or hiding
  , eiImportType :: ImportDeclarationType
  -- | The imported-as name, for qualified imports
  , eiImportedAs :: Maybe ModuleName
  } deriving (Show, Generic)

instance Serialise ExternsImport

-- | A fixity declaration in an externs file
data ExternsFixity = ExternsFixity
  {
  -- | The associativity of the operator
    efAssociativity :: Associativity
  -- | The precedence level of the operator
  , efPrecedence :: Precedence
  -- | The operator symbol
  , efOperator :: OpName 'ValueOpName
  -- | The value the operator is an alias for
  , efAlias :: Qualified (Either Ident (ProperName 'ConstructorName))
  } deriving (Show, Generic)

instance Serialise ExternsFixity

-- | A type fixity declaration in an externs file
data ExternsTypeFixity = ExternsTypeFixity
  {
  -- | The associativity of the operator
    efTypeAssociativity :: Associativity
  -- | The precedence level of the operator
  , efTypePrecedence :: Precedence
  -- | The operator symbol
  , efTypeOperator :: OpName 'TypeOpName
  -- | The value the operator is an alias for
  , efTypeAlias :: Qualified (ProperName 'TypeName)
  } deriving (Show, Generic)

instance Serialise ExternsTypeFixity

-- | A type or value declaration appearing in an externs file
data ExternsDeclaration =
  -- | A type declaration
    EDType
      { edTypeName                :: ProperName 'TypeName
      , edTypeKind                :: SourceType
      , edTypeDeclarationKind     :: TypeKind
      }
  -- | A type synonym
  | EDTypeSynonym
      { edTypeSynonymName         :: ProperName 'TypeName
      , edTypeSynonymArguments    :: [(Text, Maybe SourceType)]
      , edTypeSynonymType         :: SourceType
      }
  -- | A data constructor
  | EDDataConstructor
      { edDataCtorName            :: ProperName 'ConstructorName
      , edDataCtorOrigin          :: DataDeclType
      , edDataCtorTypeCtor        :: ProperName 'TypeName
      , edDataCtorType            :: SourceType
      , edDataCtorFields          :: [Ident]
      }
  -- | A value declaration
  | EDValue
      { edValueName               :: Ident
      , edValueType               :: SourceType
      }
  -- | A type class declaration
  | EDClass
      { edClassName               :: ProperName 'ClassName
      , edClassTypeArguments      :: [(Text, Maybe SourceType)]
      , edClassMembers            :: [(Ident, SourceType)]
      , edClassConstraints        :: [SourceConstraint]
      , edFunctionalDependencies  :: [FunctionalDependency]
      , edIsEmpty                 :: Bool
      }
  -- | An instance declaration
  | EDInstance
      { edInstanceClassName       :: Qualified (ProperName 'ClassName)
      , edInstanceName            :: Ident
      , edInstanceForAll          :: [(Text, SourceType)]
      , edInstanceKinds           :: [SourceType]
      , edInstanceTypes           :: [SourceType]
      , edInstanceConstraints     :: Maybe [SourceConstraint]
      , edInstanceChain           :: Maybe ChainId
      , edInstanceChainIndex      :: Integer
      , edInstanceNameSource      :: NameSource
      , edInstanceSourceSpan      :: SourceSpan
      }
  deriving (Show, Generic)

instance Serialise ExternsDeclaration

-- | Check whether the version in an externs file matches the currently running
-- version.
externsIsCurrentVersion :: ExternsFile -> Bool
externsIsCurrentVersion ef =
  T.unpack (efVersion ef) == showVersion Paths.version

-- | Convert an externs file back into a module
applyExternsFileToEnvironment :: ExternsFile -> Environment -> Environment
applyExternsFileToEnvironment ExternsFile{..} = flip (foldl' applyDecl) efDeclarations
  where
  applyDecl :: Environment -> ExternsDeclaration -> Environment
  applyDecl env (EDType pn kind tyKind) = env { types = M.insert (qual pn) (kind, tyKind) (types env) }
  applyDecl env (EDTypeSynonym pn args ty) = env { typeSynonyms = M.insert (qual pn) (args, ty) (typeSynonyms env) }
  applyDecl env (EDDataConstructor pn dTy tNm ty nms) = env { dataConstructors = M.insert (qual pn) (dTy, tNm, ty, nms) (dataConstructors env) }
  applyDecl env (EDValue ident ty) = env { names = M.insert (Qualified (ByModuleName efModuleName) ident) (ty, External, Defined) (names env) }
  applyDecl env (EDClass pn args members cs deps tcIsEmpty) = env { typeClasses = M.insert (qual pn) (makeTypeClassData args members cs deps tcIsEmpty) (typeClasses env) }
  applyDecl env (EDInstance className ident vars kinds tys cs ch idx ns ss) =
    env { typeClassDictionaries =
            updateMap
              (updateMap (M.insertWith (<>) (qual ident) (pure dict)) className)
              (ByModuleName efModuleName) (typeClassDictionaries env) }
    where
    dict :: NamedDict
    dict = TypeClassDictionaryInScope ch idx (qual ident) [] className vars kinds tys cs instTy

    updateMap :: (Ord k, Monoid a) => (a -> a) -> k -> M.Map k a -> M.Map k a
    updateMap f = M.alter (Just . f . fold)

    instTy :: Maybe SourceType
    instTy = case ns of
      CompilerNamed -> Just $ srcInstanceType ss vars className tys
      UserNamed -> Nothing

  qual :: a -> Qualified a
  qual = Qualified (ByModuleName efModuleName)



-- type ExternsA =
--   { imports :: Map ModuleName (Map CacheKey CacheShape)
--   , exportShapes :: Map CacheKey CacheShape
--   , exports :: Map CacheKey (WhatKindOfThingIsThisAndWhatAreItsParameters, Set (ModuleName, CacheKey))
--   }
--
-- type CacheShape = (WhatKindOfThingIsThisAndWhatAreItsParameters, Set (ModuleName, CacheKey))
--
-- -- ASSUMPTION[drathier]: this data type is opaque and only has an Eq and Ord instance. This is so that we can store it as an opaque bytestring or hash later on.
-- data WhatKindOfThingIsThisAndWhatAreItsParameters
--   = PrimType ModuleName (ProperName 'TypeName)
--   | TypeClassDictType ModuleName (ProperName 'TypeName)
--   | OwnModuleRef ModuleName (ProperName 'TypeName)
--   | CacheShapeTypeDecl
--     (ProperName 'TypeName)
--     [(Text, Maybe (Type ()))]
--     (Type ())
--   | CacheShapeForeignTypeDecl
--     (ProperName 'TypeName)
--     (Type ())
--   | CacheShapeDataDecl
--     DataDeclType
--     (ProperName 'TypeName)
--     [(Text, Maybe (Type ()))]
--     [(ProperName 'ConstructorName, [(Ident, Type ())])]
--   -- CacheShapeDataRecDecl
--   --   DataDeclType
--   --   (ProperName 'TypeName)
--   --   [(Text, Maybe (Type ()))]
--   --   [(ProperName 'ConstructorName, [(Ident, Type ())])]
--   deriving (Show, Eq, Ord, Generic)
--
-- instance Serialise CacheShape
--
-- data CacheTypeDetails = CacheTypeDetails (M.Map (ModuleName, ProperName 'TypeName) (CacheShape, CacheTypeDetails))
--   deriving (Show, Eq, Generic)
--
-- instance Serialise CacheTypeDetails


-- Declarations suitable for caching, where things like SourcePos are removed, and each ctor is isolated
data CSDataDeclaration = CSDataDeclaration DataDeclType (ProperName 'TypeName) [(Text, Maybe SourceType)] [CSDataConstructorDeclaration] (Maybe CSKindDeclaration) (Maybe CSRoleDeclaration)
  deriving (Show)
  -- |
  -- A type synonym declaration (name, arguments, type)
  --
data CSTypeSynonymDeclaration = CSTypeSynonymDeclaration (ProperName 'TypeName) [(Text, Maybe (Type ()))] (Type ()) (Maybe CSKindDeclaration)
  deriving (Show)
  -- |
  -- A kind signature declaration
  --
data CSKindDeclaration = CSKindDeclaration (Type ())
  deriving (Show)
  -- |
  -- A role declaration (name, roles)
  --
data CSRoleDeclaration = CSRoleDeclaration [Role]
  deriving (Show)
  -- |
  -- A value declaration (name, top-level binders, optional guard, value)
  --
data CSValueDeclaration = CSValueDeclaration NameKind [Binder] ToCSDB
  deriving (Show)
  -- |
  -- A declaration paired with pattern matching in let-in expression (binder, optional guard, value)
data CSBoundValueDeclaration = CSBoundValueDeclaration Binder Expr
  deriving (Show)
  -- |
  -- A minimal mutually recursive set of value declarations
  --
data CSBindingGroupDeclaration = CSBindingGroupDeclaration (NEL.NonEmpty Ident, NameKind, Expr)
  deriving (Show)
  -- |
  -- A foreign import declaration (name, type)
  --
data CSExternDeclaration = CSExternDeclaration ToCSDB
  deriving (Show)
  -- |
  -- A data type foreign import (name, kind)
  --
data CSExternDataDeclaration = CSExternDataDeclaration ToCSDB
  deriving (Show)
  -- |
  -- A fixity declaration
  --
data CSFixityDeclaration = CSFixityDeclaration (Either ValueFixity TypeFixity)
  deriving (Show)
data CSOpFixity = CSOpFixity Fixity (Qualified Ident)
  deriving (Show)
data CSCtorFixity = CSCtorFixity Fixity (Qualified (ProperName 'ConstructorName))
  deriving (Show)
data CSTyOpFixity = CSTyOpFixity Fixity (Qualified (ProperName 'TypeName))
  deriving (Show)
  -- |
  -- A module import (module name, qualified/unqualified/hiding, optional "qualified as" name)
  --
data CSImportDeclaration = CSImportDeclaration ModuleName ImportDeclarationType (Maybe ModuleName)
  deriving (Show)
  -- |
  -- A type class declaration (name, argument, implies, member declarations)
  --
data CSTypeClassDeclaration = CSTypeClassDeclaration [(Text, Maybe (Type ()))] ([Constraint ()], ToCSDB) [FunctionalDependency] [CSTypeDeclaration]
  deriving (Show)

data CSTypeDeclaration = CSTypeDeclaration Ident (Type ()) ToCSDB
  deriving (Show)
  -- |
  -- A type instance declaration (instance chain, chain index, name,
  -- dependencies, class name, instance types, member declarations)
  --
  -- The first @SourceAnn@ serves as the annotation for the entire
  -- declaration, while the second @SourceAnn@ serves as the
  -- annotation for the type class and its arguments.
data CSTypeInstanceDeclaration = CSTypeInstanceDeclaration (ChainId, Integer) ToCSDB (Qualified (ProperName 'ClassName)) ToCSDB (CSTypeInstanceBody, ToCSDB)
  deriving (Show)

data CSTypeInstanceBody
  = CSDerivedInstance
  | CSNewtypeInstance
  | CSExplicitInstance
  deriving (Show, Eq)


  -- TODO[drathier]: fix this hack
instance Eq CSDataDeclaration where
    a == b = show a == show b
instance Eq CSTypeSynonymDeclaration where
    a == b = show a == show b
instance Eq CSKindDeclaration where
    a == b = show a == show b
instance Eq CSRoleDeclaration where
    a == b = show a == show b
instance Eq CSValueDeclaration where
    a == b = show a == show b
instance Eq CSBoundValueDeclaration where
    a == b = show a == show b
instance Eq CSBindingGroupDeclaration where
    a == b = show a == show b
instance Eq CSExternDeclaration where
    a == b = show a == show b
instance Eq CSExternDataDeclaration where
    a == b = show a == show b
instance Eq CSFixityDeclaration where
    a == b = show a == show b
instance Eq CSImportDeclaration where
    a == b = show a == show b
instance Eq CSTypeClassDeclaration where
    a == b = show a == show b
instance Eq CSTypeInstanceDeclaration where
    a == b = show a == show b

instance Eq CSOpFixity where
    a == b = show a == show b
instance Eq CSCtorFixity where
    a == b = show a == show b
instance Eq CSTyOpFixity where
    a == b = show a == show b



data CSDataConstructorDeclaration
  = CSDataConstructorDeclaration
    { csdataCtorName :: !(ProperName 'ConstructorName)
    , csdataCtorFields :: ![(Ident, Type ())]
    }
  deriving (Show, Eq)

data ToCSDB
  = ToCSDB
    -- TODO[drathier]: all these Qualified contain SourcePos if it's referring to something in the current module. We generally don't want to store SourcePos, but since it's only local, we're already rebuilding the module at that point, so I'll leave it in.
    { _referencedCtors :: M.Map (Qualified (ProperName 'ConstructorName)) ()
    -- NOTE[drathier]: it's likely that we only care about the types of ctors, not types themselves, as using ctors means we care about the type shape changing, but if we just refer to the type we probably don't care what its internal structure is.
    , _referencedTypes :: M.Map (Qualified (ProperName 'TypeName)) ()
    , _referencedTypeOp :: M.Map (Qualified (OpName 'TypeOpName)) ()
    , _referencedTypeClass :: M.Map (Qualified (ProperName 'ClassName)) ()
    , _referencedValues :: M.Map (Qualified Ident) ()
    , _referencedValueOp :: M.Map (Qualified (OpName 'ValueOpName)) ()
    }
  deriving (Show, Eq)


instance Semigroup ToCSDB where
  ToCSDB a1 a2 a3 a4 a5 a6 <> ToCSDB b1 b2 b3 b4 b5 b6 = ToCSDB (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5) (a6 <> b6)

instance Monoid ToCSDB where
  mempty = ToCSDB mempty mempty mempty mempty mempty mempty

-- NOTE[drathier]: yes, I know Language.PureScript.AST.Traversals exists, but since not all things are newtype wrapped, I would've missed some cases using that, e.g. kind signatures
class ToCS a b | a -> b where
  toCS :: a -> State ToCSDB b

instance ToCS DataConstructorDeclaration CSDataConstructorDeclaration where
  toCS (DataConstructorDeclaration _ ctorName ctorFields) =
    do
      ctorFields2 <-
        ctorFields
          & traverse
            (\(ident, typeWithSrcAnn) ->
              do
                let t2 = const () <$> typeWithSrcAnn
                toCS typeWithSrcAnn
                pure (ident, t2)
            )
      pure $
        CSDataConstructorDeclaration
          ctorName
          ctorFields2

instance ToCS GuardedExpr () where
  toCS (GuardedExpr guards expr) =
    do
      nguards <- traverse toCS guards
      nexpr <- toCS expr
      pure ()

instance ToCS Guard () where
  toCS guard =
    case guard of
        ConditionGuard expr -> toCS expr
        PatternGuard binder expr -> do
          toCS binder
          toCS expr

instance ToCS Expr () where
  toCS expr =
    case expr of
      Literal _ lit -> toCS lit
      UnaryMinus _ e -> toCS e
      BinaryNoParens e1 e2 e3 -> do
        toCS e1
        toCS e2
        toCS e3
      Parens e -> toCS e
      Accessor _ e -> toCS e
      ObjectUpdate e obj -> do
        toCS e
        traverse_ (traverse_ toCS) obj
      ObjectUpdateNested e tree -> do
         toCS e
         traverse_ toCS tree
      Abs binder e -> do
        toCS binder
        toCS e
      App e1 e2 -> do
        toCS e1
        toCS e2
      Unused e -> toCS e
      Var _ qIdent -> csdbPutIdent qIdent
      Op _ qValueOpName -> csdbPutValueOp qValueOpName
      IfThenElse e1 e2 e3 -> do
        toCS e1
        toCS e2
        toCS e3
      Constructor _ qCtorName -> csdbPutCtor qCtorName
      Case exprs cases -> do
        traverse_ toCS exprs
        traverse_ toCS cases
      TypedValue _ e sourceType -> do
        toCS e
        toCS sourceType

      Let whereOrLet decls inExpr -> do
        toCS decls
        toCS inExpr
      Do _ doNotationElems -> do
        traverse_ toCS doNotationElems

      Ado _ doNotationElems inExpr -> do
        traverse_ toCS doNotationElems
        toCS inExpr

      TypeClassDictionary _ _ _ -> error "[drathier]: should be unreachable, all TypeClassDictionary ctors should have been expanded already"
      DeferredDictionary _ _ -> error "[drathier]should be unreachable, all DeferredDictionary ctors should have been expanded already"
      DerivedInstancePlaceholder _ _ -> error "[drathier]should be unreachable, all DerivedInstancePlaceholder ctors should have been expanded already"
      AnonymousArgument -> error "[drathier]: shshould be unreachable, all AnonymousArgument ctors should have been expanded already"
      Hole _ -> error "[drathier]: should be unreachable, all Hole ctors should have been expanded already"
      PositionedValue _ _ e -> toCS e

instance ToCS a () => ToCS (Literal a) () where
  toCS expr =
    case expr of
      NumericLiteral _ -> pure ()
      StringLiteral _ -> pure ()
      CharLiteral _ -> pure ()
      BooleanLiteral _ -> pure ()
      ArrayLiteral arr -> traverse_ toCS arr
      ObjectLiteral obj -> traverse_ (traverse_ toCS) obj


instance ToCS DoNotationElement () where
  toCS doNotationElement =
    case doNotationElement of
      DoNotationValue e -> toCS e
      DoNotationBind binder e -> do
        toCS binder
        toCS e
      DoNotationLet decls -> do
        toCS decls
      PositionedDoNotationElement _ _ elem -> toCS elem

instance ToCS Binder () where
  toCS binder =
    case binder of
      NullBinder -> pure ()
      LiteralBinder _ literal -> toCS literal
      VarBinder _ _ -> pure ()
      ConstructorBinder _ ctorName binders -> do
        csdbPutCtor ctorName
        traverse_ toCS binders
      OpBinder _ _ -> error "[drathier]: should be unreachable, all OpBinder ctors should have been desugared already"
      BinaryNoParensBinder _ _ _ -> error "[drathier]: should be unreachable, all BinaryNoParensBinder ctors should have been desugared already"
      ParensInBinder _ -> error "[drathier]: should be unreachable, all ParensInBinder ctors should have been desugared already"
      NamedBinder _ _ innerBinder -> toCS innerBinder
      PositionedBinder _ _ innerBinder -> toCS innerBinder
      TypedBinder sourceType innerBinder -> do
        toCS sourceType
        toCS innerBinder

instance ToCS CaseAlternative () where
  toCS (CaseAlternative binders guardedExprs) = do
    traverse_ toCS binders
    traverse_ toCS guardedExprs

instance ToCS [Declaration] () where
  toCS ds = do
    -- TODO[drathier]: lifting CSDB values out of DB like this feels weird. Should CSDB and DB be the same type? Should we use ToCS for e.g. findDeps too?
    findDeps ds
    & traverse_ (\(_, DB dataOrNewtypeDecls typeSynonymDecls valueDecls externDecls externDataDecls opFixity ctorFixity tyOpFixity tyClassDecls tyClassInstanceDecls) -> do
      dataOrNewtypeDecls & traverse_ (traverse_ (\(csdb, _) -> modify (<> csdb)))
      valueDecls & traverse_ (traverse_ (\(CSValueDeclaration _ _ csdb) -> modify (<> csdb)))
      externDecls & traverse_ (traverse_ (\(CSExternDeclaration csdb) -> modify (<> csdb)))
      externDataDecls & traverse_ (traverse_ (\(CSExternDataDeclaration csdb) -> modify (<> csdb)))
      tyClassDecls & traverse_ (traverse_ (\(CSTypeClassDeclaration _ (_, csdb) _ tyDeps) ->
        do
          modify (<> csdb)
          tyDeps & traverse_ (\(CSTypeDeclaration _ _ csdb) -> modify (<> csdb))
        ))
      tyClassInstanceDecls & traverse_ (traverse_ (\(CSTypeInstanceDeclaration _ csdb1 _ csdb2 (_, csdb3)) ->
          modify (<> csdb1 <> csdb2 <> csdb3)
        ))
    )

instance ToCS (Type SourceAnn) () where
  toCS t = storeTypeRefs t

instance ToCS (Type ()) () where
  toCS = storeTypeRefs

instance ToCS (Constraint SourceAnn) () where
  toCS = storeConstraintTypes

storeConstraintTypes :: Constraint a -> State ToCSDB ()
storeConstraintTypes (Constraint _ refTypeClass kindArgs targs mdata) = do
  csdbPutTypeClass refTypeClass
  traverse_ storeTypeRefs kindArgs
  traverse_ storeTypeRefs targs

-- CSDB put helpers

csdbPutCtor :: Qualified (ProperName 'ConstructorName) -> State ToCSDB ()
csdbPutCtor refCtor =
  modify
   (\v ->
    v {
      _referencedCtors =
       M.insert
        refCtor
        ()
        (_referencedCtors v)
     }
   )

csdbPutType :: Qualified (ProperName 'TypeName) -> State ToCSDB ()
csdbPutType refType =
  modify
   (\v ->
    v {
      _referencedTypes =
       M.insert
        refType
        ()
        (_referencedTypes v)
     }
   )

csdbPutTypeOp :: Qualified (OpName 'TypeOpName) -> State ToCSDB ()
csdbPutTypeOp refTypeOp =
  modify
   (\v ->
    v {
      _referencedTypeOp =
       M.insert
        refTypeOp
        ()
        (_referencedTypeOp v)
     }
   )

csdbPutTypeClass :: Qualified (ProperName 'ClassName) -> State ToCSDB ()
csdbPutTypeClass refTypeClass =
  modify
   (\v ->
    v {
      _referencedTypeClass =
       M.insert
        refTypeClass
        ()
        (_referencedTypeClass v)
     }
   )

csdbPutIdent :: Qualified Ident -> State ToCSDB ()
csdbPutIdent refValue =
  modify
   (\v ->
    v {
      _referencedValues =
       M.insert
        refValue
        ()
        (_referencedValues v)
     }
   )

csdbPutValueOp :: Qualified (OpName 'ValueOpName) -> State ToCSDB ()
csdbPutValueOp refOp =
  modify
   (\v ->
    v {
      _referencedValueOp =
       M.insert
        refOp
        ()
        (_referencedValueOp v)
     }
   )

-- DB put helpers

dbPutDataDeclaration :: ProperName 'TypeName -> ToCSDB -> CSDataDeclaration -> State DB ()
dbPutDataDeclaration tname nctorsDB csDataDeclaration =
  modify
   (\db ->
    db
      {
        _dataOrNewtypeDecls =
           M.insertWith (<>)
            tname
            [(nctorsDB, csDataDeclaration)]
            (_dataOrNewtypeDecls db)
      }
   )

dbPutTypeSynonymDeclaration :: ProperName 'TypeName -> CSTypeSynonymDeclaration -> State DB ()
dbPutTypeSynonymDeclaration tname csDataDeclaration =
  modify
   (\db ->
    db
      {
        _typeSynonymDecls =
           M.insertWith (<>)
            tname
            [csDataDeclaration]
            (_typeSynonymDecls db)
      }
   )


dbPutValueDeclaration :: Ident -> CSValueDeclaration -> State DB ()
dbPutValueDeclaration ident csValueDeclaration =
  modify
   (\db ->
    db
      {
        _valueDecls =
           M.insertWith (<>)
            ident
            [csValueDeclaration]
            (_valueDecls db)
      }
   )


dbPutExternDeclaration :: Ident -> CSExternDeclaration -> State DB ()
dbPutExternDeclaration ident csExternDeclaration =
  modify
   (\db ->
    db
      {
        _externDecls =
           M.insertWith (<>)
            ident
            [csExternDeclaration]
            (_externDecls db)
      }
   )


dbPutExternDataDeclaration :: ProperName 'TypeName -> CSExternDataDeclaration -> State DB ()
dbPutExternDataDeclaration tname csExternDataDeclaration =
  modify
   (\db ->
    db
      {
        _externDataDecls =
           M.insertWith (<>)
            tname
            [csExternDataDeclaration]
            (_externDataDecls db)
      }
   )


dbPutOpFixity :: OpName 'ValueOpName -> CSOpFixity -> State DB ()
dbPutOpFixity tname csOpFixity =
  modify
   (\db ->
    db
      {
        _opFixity =
           M.insertWith (<>)
            tname
            [csOpFixity]
            (_opFixity db)
      }
   )


dbPutCtorFixity :: OpName 'ValueOpName -> CSCtorFixity -> State DB ()
dbPutCtorFixity opName csCtorFixity =
  modify
   (\db ->
    db
      {
        _ctorFixity =
           M.insertWith (<>)
            opName
            [csCtorFixity]
            (_ctorFixity db)
      }
   )


dbPutTyOpFixity :: OpName 'TypeOpName -> CSTyOpFixity -> State DB ()
dbPutTyOpFixity tyOpName csTyOpFixity =
  modify
   (\db ->
    db
      {
        _tyOpFixity =
           M.insertWith (<>)
            tyOpName
            [csTyOpFixity]
            (_tyOpFixity db)
      }
   )

dbPutTypeClassDeclaration :: ProperName 'ClassName -> CSTypeClassDeclaration -> State DB ()
dbPutTypeClassDeclaration className csTypeClassDeclaration =
  modify
   (\db ->
    db
      {
        _tyClassDecls =
           M.insertWith (<>)
            className
            [csTypeClassDeclaration]
            (_tyClassDecls db)
      }
   )

dbPutTypeInstanceDeclaration :: Ident -> CSTypeInstanceDeclaration -> State DB ()
dbPutTypeInstanceDeclaration ident csTypeInstanceDeclaration =
  modify
   (\db ->
    db
      {
        _tyClassInstanceDecls =
           M.insertWith (<>)
            ident
            [csTypeInstanceDeclaration]
            (_tyClassInstanceDecls db)
      }
   )

storeTypeRefs :: Type a -> State ToCSDB ()
storeTypeRefs t =
  case t of
    TUnknown _ _ -> pure ()
    TypeVar _ _ -> pure ()
    TypeLevelString _ _ -> pure ()
    TypeLevelInt _ _ -> pure ()
    TypeWildcard _ _ -> pure ()
    TypeConstructor _ refType -> do
      -- [drathier]: this seems to be the type, not the constructor, which makes much more sense at the type level
      csdbPutType refType

    TypeOp _ refTypeOp -> do
      -- TODO[drathier]: is this ctor unused here? docs for it say it's desugared to a TypeConstructor
      csdbPutTypeOp refTypeOp

    TypeApp _ t1 t2 -> do
      storeTypeRefs t1
      storeTypeRefs t2

    KindApp _ t1 t2 -> do
      storeTypeRefs t1
      storeTypeRefs t2

    ForAll _ _ mt1 t2 _ -> do
      traverse_ storeTypeRefs mt1
      storeTypeRefs t2

    ConstrainedType _ constraint t1 -> do
      storeConstraintTypes constraint
      storeTypeRefs t1

    Skolem _ _ mt1 _ _ -> do
      traverse_ storeTypeRefs mt1
      pure ()

    REmpty a -> pure ()

    RCons _ _ t1 t2 -> do
      storeTypeRefs t1
      storeTypeRefs t2

    KindedType _ t1 t2 -> do
      storeTypeRefs t1
      storeTypeRefs t2

    BinaryNoParensType _ t1 t2 t3 -> do
      storeTypeRefs t1
      storeTypeRefs t2
      storeTypeRefs t3

    ParensInType _ t1 -> do
      storeTypeRefs t1


data DB
  = DB
    -- what things did we find?
    -- NOTE[drathier]: this gathers a list of direct dependencies, not transitive dependencies, and it doesn't fetch the shape of the dependencies. It's just the set of things we depend on, for us to fetch later.
    -- TODO[drathier]: make sure all these lists are singletons
    { _dataOrNewtypeDecls :: M.Map (ProperName 'TypeName) [(ToCSDB, CSDataDeclaration)]
    , _typeSynonymDecls :: M.Map (ProperName 'TypeName) [CSTypeSynonymDeclaration]
    , _valueDecls :: M.Map Ident [CSValueDeclaration] -- TODO[drathier]: ToCSDB here too?
    , _externDecls :: M.Map Ident [CSExternDeclaration]
    , _externDataDecls :: M.Map (ProperName 'TypeName) [CSExternDataDeclaration]
    , _opFixity :: M.Map (OpName 'ValueOpName) [CSOpFixity]
    , _ctorFixity :: M.Map (OpName 'ValueOpName) [CSCtorFixity]
    , _tyOpFixity :: M.Map (OpName 'TypeOpName) [CSTyOpFixity]
    , _tyClassDecls :: M.Map (ProperName 'ClassName) [CSTypeClassDeclaration]
    , _tyClassInstanceDecls :: M.Map Ident [CSTypeInstanceDeclaration]
    }
  deriving (Show, Eq)

instance Semigroup DB where
  DB a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 <> DB b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 = DB (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5) (a6 <> b6) (a7 <> b7) (a8 <> b8) (a9 <> b9) (a10 <> b10)

instance Monoid DB where
  mempty = DB mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty



data CacheKey
  = CNDataNewtypeDecl (ProperName 'TypeName)



-- data DataConstructorDeclaration = DataConstructorDeclaration
--   { dataCtorAnn :: !SourceAnn
--   , dataCtorName :: !(ProperName 'ConstructorName)
--   , dataCtorFields :: ![(Ident, SourceType)]
--   } deriving (Show, Eq)

-- type SourceType = Type SourceAnn

findDeps :: [Declaration] -> [(Declaration, DB)]
findDeps ds =
  let
    (kindsMap, rolesMap, otherDs) =
      ds
      & flattenDecls
      & foldl (\(akind, arole, aother) d ->
        let
          addKind key value = (M.insert key value akind, arole, aother)
          addRole key value = (akind, M.insert key value arole, aother)
          addOther other = (akind, arole, other : aother)
        in
          case d of
            KindDeclaration _ kindSignatureFor referencedName stype ->
              addKind (kindSignatureFor, referencedName) (CSKindDeclaration (const () <$> stype))
            RoleDeclaration (RoleDeclarationData _ tname roles) ->
              addRole tname (CSRoleDeclaration roles)
            d -> addOther d
        )
      (mempty, mempty, mempty)

    getKind :: KindSignatureFor -> ProperName 'TypeName -> Maybe CSKindDeclaration
    getKind kindSignatureFor tname =
      M.lookup (kindSignatureFor, tname) kindsMap

    getRole :: ProperName 'TypeName -> Maybe CSRoleDeclaration
    getRole tname =
      M.lookup tname rolesMap
  in
  otherDs
    <&> (\a -> do
      (a, mempty & execState (findDepsImpl getKind getRole a))
     )
    & filter (\(_, db) -> db /= mempty)

findDepsImpl
  :: (KindSignatureFor -> ProperName 'TypeName -> Maybe CSKindDeclaration)
  -> (ProperName 'TypeName -> Maybe CSRoleDeclaration)
  -> Declaration -> State DB ()
findDepsImpl getKind getRole d =
  -- data Declaration
  case d of
    -- DataDeclaration SourceAnn DataDeclType (ProperName 'TypeName) [(Text, Maybe SourceType)] [DataConstructorDeclaration]
    DataDeclaration _ dataOrNewtype tname targs ctors -> do
      let (nctorsValue, nctorsDB) = mempty & runState (traverse toCS ctors)

      let mkind =
            case dataOrNewtype of
              Data -> getKind DataSig tname
              Newtype -> getKind NewtypeSig tname

      dbPutDataDeclaration tname nctorsDB (CSDataDeclaration dataOrNewtype tname targs nctorsValue mkind (getRole tname))
      -- pure $ f (show ("DataDeclaration", tname)) $
      --   show ("DataDeclaration", dataOrNewtype, tname, targs, ctors)
    -- DataBindingGroupDeclaration (NEL.NonEmpty Declaration)
    DataBindingGroupDeclaration _ ->
      error "[drathier]: should be unreachable, all DataBindingGroupDeclaration ctors should have been flattened earlier"
    -- TypeSynonymDeclaration SourceAnn (ProperName 'TypeName) [(Text, Maybe SourceType)] SourceType
    TypeSynonymDeclaration _ tname targs stype -> do
      -- TODO[drathier]: find some source code that leaves targs with a Just SourceType.
      let ntargs =
            targs <&>
                ( \case
                    (targName, Just _) -> error "unhandled case; TypeSynonymDeclaration targ is Just"
                    (targName, Nothing) ->
                      (targName, Nothing)
                )
      let nstype = const () <$> stype
      dbPutTypeSynonymDeclaration tname (CSTypeSynonymDeclaration tname ntargs nstype (getKind TypeSynonymSig tname))

    -- KindDeclaration SourceAnn KindSignatureFor (ProperName 'TypeName) SourceType
    KindDeclaration _ _ _ _ ->
      error "[drathier]: should be unreachable, all KindDeclaration ctors should have been filtered out earlier"

    -- RoleDeclaration {-# UNPACK #-} !RoleDeclarationData
    RoleDeclaration _ ->
      -- ASSUMPTION[drathier]: got this compiler error when testing, assuming it to be true forever "Role declarations are only supported for data types, not for type synonyms nor type classes." We'll likely incorrectly  cache things wrt this if this changes in the future. Testing also shows that it works fine for newtypes, so I'm supporting that.
      error "[drathier]: should be unreachable, all RoleDeclaration ctors should have been filtered out earlier"
    -- TypeDeclaration {-# UNPACK #-} !TypeDeclarationData
    TypeDeclaration _ ->
      error "ASSUMPTION[drathier]: should be unreachable, all TypeDeclaration ctors should have been extracted earlier"
    -- ValueDeclaration {-# UNPACK #-} !(ValueDeclarationData [GuardedExpr])
    ValueDeclaration (ValueDeclarationData _ ident namekind binders expr) -> do
      -- TODO[drathier]: do we really need expr in here too? Yes, we need to know what modules its value and type refers to at least.
      let !(nexprValue, nexprDB) = mempty & runState (traverse toCS expr)
      dbPutValueDeclaration ident (CSValueDeclaration namekind binders nexprDB)

    -- BoundValueDeclaration SourceAnn Binder Expr
    BoundValueDeclaration _ _ _ ->
      error "ASSUMPTION[drathier]: should be unreachable, all BoundValueDeclaration ctors should have been desugared earlier"
    -- BindingGroupDeclaration (NEL.NonEmpty ((SourceAnn, Ident), NameKind, Expr))
    BindingGroupDeclaration _ ->
      error "ASSUMPTION[drathier]: should be unreachable, all BindingGroupDeclaration ctors should have been desugared earlier"
    -- ExternDeclaration SourceAnn Ident SourceType
    ExternDeclaration _ ident sourceType -> do
      let !(_, ntypeDB) = mempty & runState (toCS sourceType)
      dbPutExternDeclaration ident (CSExternDeclaration ntypeDB)

    -- ExternDataDeclaration SourceAnn (ProperName 'TypeName) SourceType
    ExternDataDeclaration _ tname sourceType -> do
      let !(_, ntypeDB) = mempty & runState (toCS sourceType)
      dbPutExternDataDeclaration tname (CSExternDataDeclaration ntypeDB)

    -- FixityDeclaration SourceAnn (Either ValueFixity TypeFixity)
    FixityDeclaration _ eitherValueType ->
      case eitherValueType of
        -- ValueFixityDeclaration :: SourceAnn -> Fixity -> Qualified (Either Ident (ProperName 'ConstructorName)) -> OpName 'ValueOpName -> Declaration
        Left (ValueFixity fixity (N.Qualified qBy (Left ident)) localOpName) ->
          dbPutOpFixity localOpName (CSOpFixity fixity (N.Qualified qBy ident))
        Left (ValueFixity fixity (N.Qualified qBy (Right ctor)) localCtorName) ->
          dbPutCtorFixity localCtorName (CSCtorFixity fixity (N.Qualified qBy ctor))
        -- TypeFixityDeclaration :: SourceAnn -> Fixity -> Qualified (ProperName 'TypeName) -> OpName 'TypeOpName -> Declaration
        Right (TypeFixity fixity tname tyOpName) ->
          dbPutTyOpFixity tyOpName (CSTyOpFixity fixity tname)

    -- ImportDeclaration SourceAnn ModuleName ImportDeclarationType (Maybe ModuleName)
    ImportDeclaration _ _ _ _ -> pure ()
    -- TypeClassDeclaration SourceAnn (ProperName 'ClassName) [(Text, Maybe SourceType)] [SourceConstraint] [FunctionalDependency] [Declaration]
    TypeClassDeclaration _ className targs constraints fnDeps decls -> do
      ndecls <- decls & traverse (\decl ->
        case decl of
          TypeDeclaration (TypeDeclarationData _ ident tipe) -> do
            let (ntipe, ntipeDB) = mempty & runState (toCS tipe)
            pure $ CSTypeDeclaration ident (const () <$> tipe) ntipeDB
        )

      -- TODO[drathier]: test the constraintKindArgs and constraintData fields of Constraint. I couldn't figure out a source input that would put anything in those fields.

      let (_, nconstraintsdb) = mempty & runState (traverse toCS constraints)
      let nconstraints = fmap (const ()) <$> constraints
      let ntargs = targs <&> (\(a, sourceType) -> (a, fmap (const ()) <$> sourceType))
      let !_ = nconstraints <&>
            (\case
              Constraint _ _ [] _ _ -> ()
              v -> error ("ASSUMPTION[drathier]: the constraintKindArgs field of constraints for type classes is always empty." ++ show v)
            )
      dbPutTypeClassDeclaration className (CSTypeClassDeclaration ntargs (nconstraints, nconstraintsdb) fnDeps ndecls)

    -- TypeInstanceDeclaration SourceAnn SourceAnn ChainId Integer (Either Text Ident) [SourceConstraint] (Qualified (ProperName 'ClassName)) [SourceType] TypeInstanceBody
    TypeInstanceDeclaration _ _ chainId chainIdIndex eitherTextIdentInstanceName dependencySourceConstraints className instanceSourceTypes derivedNewtypeExplicit -> do
      let !(_, ndependencySourceConstraintsDB) = mempty & runState (traverse toCS dependencySourceConstraints)
      let !(_, ninstanceSourceTypesDB) = mempty & runState (traverse toCS instanceSourceTypes)
      let !(nderivedNewtypeExplicitNoDecls, nderivedNewtypeExplicit) = mempty & runState (
                case derivedNewtypeExplicit of
                  DerivedInstance -> pure CSDerivedInstance
                  NewtypeInstance -> pure CSNewtypeInstance
                  ExplicitInstance decls ->
                    do
                      toCS decls
                      pure CSExplicitInstance
                )
      let !_ = dependencySourceConstraints <&>
            (\case
              Constraint _ _ [] _ _ -> ()
              v -> error ("ASSUMPTION[drathier]: the constraintKindArgs field of constraints for type class instances is always empty." ++ show v)
            )
      let instanceName =
            case eitherTextIdentInstanceName of
              Left v -> error "ASSUMPTION[drathier]: we'll never get a Text value here; even generated instances have Idents"
              Right v -> v
      dbPutTypeInstanceDeclaration instanceName (CSTypeInstanceDeclaration (chainId, chainIdIndex) ndependencySourceConstraintsDB className ninstanceSourceTypesDB (nderivedNewtypeExplicitNoDecls, nderivedNewtypeExplicit))


-- | Generate an externs file for all declarations in a module.
--
-- The `Map Ident Ident` argument should contain any top-level `GenIdent`s that
-- were rewritten to `Ident`s when the module was compiled; this rewrite only
-- happens in the CoreFn, not the original module AST, so it needs to be
-- applied to the exported names here also. (The appropriate map is returned by
-- `L.P.Renamer.renameInModule`.)
moduleToExternsFile :: Module -> Environment -> M.Map Ident Ident -> ExternsFile
moduleToExternsFile (Module _ _ _ _ Nothing) _ _ = internalError "moduleToExternsFile: module exports were not elaborated"
-- data Module = Module SourceSpan [Comment] ModuleName [Declaration] (Maybe [DeclarationRef])
moduleToExternsFile (Module ss _ mn ds (Just exps)) env renamedIdents =
  let
    sortDsByCtor :: Foldable f => f Declaration -> M.Map String [Declaration]
    sortDsByCtor dsx =
      foldr sortDsByCtorImpl mempty dsx
    sortDsByCtorImpl :: Declaration -> M.Map String [Declaration] -> M.Map String [Declaration]
    sortDsByCtorImpl d res =
      case d of
        DataDeclaration _ _ _ _ _ -> M.insertWith (<>) "DataDeclaration" [d] res
        DataBindingGroupDeclaration _ -> M.insertWith (<>) "DataBindingGroupDeclaration" [d] res
        TypeSynonymDeclaration _ _ _ _ -> M.insertWith (<>) "TypeSynonymDeclaration" [d] res
        KindDeclaration _ _ _ _ -> M.insertWith (<>) "KindDeclaration" [d] res
        RoleDeclaration _ -> M.insertWith (<>) "RoleDeclaration" [d] res
        TypeDeclaration _ -> M.insertWith (<>) "TypeDeclaration" [d] res
        ValueDeclaration _ -> M.insertWith (<>) "ValueDeclaration" [d] res
        BoundValueDeclaration _ _ _ -> M.insertWith (<>) "BoundValueDeclaration" [d] res
        BindingGroupDeclaration _ -> M.insertWith (<>) "BindingGroupDeclaration" [d] res
        ExternDeclaration _ _ _ -> M.insertWith (<>) "ExternDeclaration" [d] res
        ExternDataDeclaration _ _ _ -> M.insertWith (<>) "ExternDataDeclaration" [d] res
        FixityDeclaration _ _ -> M.insertWith (<>) "FixityDeclaration" [d] res
        ImportDeclaration _ _ _ _ -> M.insertWith (<>) "ImportDeclaration" [d] res
        TypeClassDeclaration _ _ _ _ _ _ -> M.insertWith (<>) "TypeClassDeclaration" [d] res
        TypeInstanceDeclaration _ _ _ _ _ _ _ _ _ -> M.insertWith (<>) "TypeInstanceDeclaration" [d] res

    sds = sortDsByCtor ds


  in

  let !_ = trace (show ("###moduleToExternsFile mn", mn)) () in
  let !_ = trace (show ("###moduleToExternsFile ds.BoundValueDeclaration", M.lookup "BoundValueDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile ds.BindingGroupDeclaration", M.lookup "BindingGroupDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile ds.ExternDeclaration", M.lookup "ExternDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile ds.ExternDataDeclaration", M.lookup "ExternDataDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile ds.FixityDeclaration", M.lookup "FixityDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile ds.ImportDeclaration", M.lookup "ImportDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile ds.TypeClassDeclaration", M.lookup "TypeClassDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile ds.TypeInstanceDeclaration", M.lookup "TypeInstanceDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile ds.DataDeclaration", M.lookup "DataDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile ds.DataBindingGroupDeclaration", M.lookup "DataBindingGroupDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile ds.TypeSynonymDeclaration", M.lookup "TypeSynonymDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile ds.KindDeclaration", M.lookup "KindDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile ds.RoleDeclaration", M.lookup "RoleDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile ds.TypeDeclaration", M.lookup "TypeDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile ds.ValueDeclaration", M.lookup "ValueDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile exps", exps)) () in
  let !_ = trace (show ("###moduleToExternsFile renamedIdents", renamedIdents)) () in
  let !_ = trace (show ("-------")) () in
  let !_ = trace (sShow ("###moduleToExternsFile findDeps", findDeps ds)) () in
  ExternsFile{..}
  where
  efVersion       = T.pack (showVersion Paths.version)
  efModuleName    = mn
  efExports       = map renameRef exps
  efImports       = mapMaybe importDecl ds
  efFixities      = mapMaybe fixityDecl ds
  efTypeFixities  = mapMaybe typeFixityDecl ds
  efDeclarations  = concatMap toExternsDeclaration exps
  efSourceSpan    = ss

  fixityDecl :: Declaration -> Maybe ExternsFixity
  fixityDecl (ValueFixityDeclaration _ (Fixity assoc prec) name op) =
    fmap (const (ExternsFixity assoc prec op name)) (find ((== Just op) . getValueOpRef) exps)
  fixityDecl _ = Nothing

  typeFixityDecl :: Declaration -> Maybe ExternsTypeFixity
  typeFixityDecl (TypeFixityDeclaration _ (Fixity assoc prec) name op) =
    fmap (const (ExternsTypeFixity assoc prec op name)) (find ((== Just op) . getTypeOpRef) exps)
  typeFixityDecl _ = Nothing

  importDecl :: Declaration -> Maybe ExternsImport
  importDecl (ImportDeclaration _ m mt qmn) = Just (ExternsImport m mt qmn)
  importDecl _ = Nothing

  toExternsDeclaration :: DeclarationRef -> [ExternsDeclaration]
  toExternsDeclaration (TypeRef _ pn dctors) =
    case Qualified (ByModuleName mn) pn `M.lookup` types env of
      Nothing -> internalError "toExternsDeclaration: no kind in toExternsDeclaration"
      Just (kind, TypeSynonym)
        | Just (args, synTy) <- Qualified (ByModuleName mn) pn `M.lookup` typeSynonyms env -> [ EDType pn kind TypeSynonym, EDTypeSynonym pn args synTy ]
      Just (kind, ExternData rs) -> [ EDType pn kind (ExternData rs) ]
      Just (kind, tk@(DataType _ _ tys)) ->
        EDType pn kind tk : [ EDDataConstructor dctor dty pn ty args
                            | dctor <- fromMaybe (map fst tys) dctors
                            , (dty, _, ty, args) <- maybeToList (Qualified (ByModuleName mn) dctor `M.lookup` dataConstructors env)
                            ]
      _ -> internalError "toExternsDeclaration: Invalid input"
  toExternsDeclaration (ValueRef _ ident)
    | Just (ty, _, _) <- Qualified (ByModuleName mn) ident `M.lookup` names env
    = [ EDValue (lookupRenamedIdent ident) ty ]
  toExternsDeclaration (TypeClassRef _ className)
    | let dictName = dictTypeName . coerceProperName $ className
    , Just TypeClassData{..} <- Qualified (ByModuleName mn) className `M.lookup` typeClasses env
    , Just (kind, tk) <- Qualified (ByModuleName mn) (coerceProperName className) `M.lookup` types env
    , Just (dictKind, dictData@(DataType _ _ [(dctor, _)])) <- Qualified (ByModuleName mn) dictName `M.lookup` types env
    , Just (dty, _, ty, args) <- Qualified (ByModuleName mn) dctor `M.lookup` dataConstructors env
    = [ EDType (coerceProperName className) kind tk
      , EDType dictName dictKind dictData
      , EDDataConstructor dctor dty dictName ty args
      , EDClass className typeClassArguments typeClassMembers typeClassSuperclasses typeClassDependencies typeClassIsEmpty
      ]
  toExternsDeclaration (TypeInstanceRef ss' ident ns)
    = [ EDInstance tcdClassName (lookupRenamedIdent ident) tcdForAll tcdInstanceKinds tcdInstanceTypes tcdDependencies tcdChain tcdIndex ns ss'
      | m1 <- maybeToList (M.lookup (ByModuleName mn) (typeClassDictionaries env))
      , m2 <- M.elems m1
      , nel <- maybeToList (M.lookup (Qualified (ByModuleName mn) ident) m2)
      , TypeClassDictionaryInScope{..} <- NEL.toList nel
      ]
  toExternsDeclaration _ = []

  renameRef :: DeclarationRef -> DeclarationRef
  renameRef = \case
    ValueRef ss' ident -> ValueRef ss' $ lookupRenamedIdent ident
    TypeInstanceRef ss' ident _ | not $ isPlainIdent ident -> TypeInstanceRef ss' (lookupRenamedIdent ident) CompilerNamed
    other -> other

  lookupRenamedIdent :: Ident -> Ident
  lookupRenamedIdent = flip (join M.findWithDefault) renamedIdents

externsFileName :: FilePath
externsFileName = "externs.cbor"
