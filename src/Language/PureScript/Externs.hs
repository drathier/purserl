{-# LANGUAGE FunctionalDependencies #-}
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
  , DB
  , dbDiffDiff
  , dbOpaqueDiffDiff
  ) where

import Prelude

import Codec.Serialise (Serialise, serialise)
import Control.Monad (join)
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.List (foldl', find)
import Data.Foldable (fold)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Version (showVersion)
import qualified Data.Map as M
import qualified Data.Map.Merge.Strict as M
import qualified Data.List.NonEmpty as NEL
import qualified Language.PureScript.Make.Cache as Cache

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
import qualified Data.ByteString.UTF8 as BS8
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as BS
import qualified Crypto.Hash as Hash
import qualified Data.ByteArray.Encoding as BAE

newtype SerializationFormat a = SerializationFormat a
  deriving (Show, Eq, Generic)

instance Serialise a => Serialise (SerializationFormat a)
instance Monoid a => Monoid (SerializationFormat a) where
  mempty = SerializationFormat mempty
instance Semigroup a => Semigroup (SerializationFormat a) where
  SerializationFormat a <> SerializationFormat b = SerializationFormat (a <> b)

toSerialized :: a -> SerializationFormat a
toSerialized a = SerializationFormat a

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
  , efUpstreamCacheShapes :: M.Map ModuleName DBOpaque
  -- ^ Shapes of things dependend upon by this module
  , efOurCacheShapes :: DBOpaque
  -- ^ Shapes of things in this module
  } deriving (Show, Generic)

instance Serialise ExternsFile

instance Eq ExternsFile where
  a == b = serialise a == serialise b

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


-- Declarations suitable for caching, where things like SourcePos are removed, and each ctor is isolated
-- Only type-level details, for when ctors aren't used.
data CSDataDeclarationTypeOnly =
  CSDataDeclarationTypeOnly
    { csDataDeclDataOrNewtype :: DataDeclType
    , csDataDeclName :: ProperName 'TypeName
    , csDataDeclTargs :: [(Text, Maybe SourceType)]
    , csDataDeclKind :: Maybe CSKindDeclaration
    , csDataDeclRole :: Maybe CSRoleDeclaration
    }
  deriving (Show, Generic, Eq)

-- Only value-level details. Only needed when ctors are used. Not useful without its corresponding CSDataDeclarationTypeOnly.
data CSDataConstructorDeclaration
  = CSDataConstructorDeclaration
    { csDataCtorName :: !(ProperName 'ConstructorName)
    , csDataCtorFields :: ![(Ident, Type ())]
    }
  deriving (Show, Eq, Generic)

-- Both type-level and value-level details together, for when ctors are in use.
data CSDataDeclarationWithCtors =
  CSDataDeclarationWithCtors
    { csDataDeclTypeOnly ::  CSDataDeclarationTypeOnly
    , csDataDeclCtorDecls :: [CSDataConstructorDeclaration]
    }
  deriving (Show, Generic, Eq)

instance Serialise CSDataDeclarationTypeOnly
instance Serialise CSDataConstructorDeclaration
instance Serialise CSDataDeclarationWithCtors

  -- |
  -- A type synonym declaration (name, arguments, type)
  --
data CSTypeSynonymDeclaration = CSTypeSynonymDeclaration (ProperName 'TypeName) [(Text, Maybe (Type ()))] (Type ()) ToCSDB (Maybe CSKindDeclaration)
  deriving (Show, Generic, Eq)

instance Serialise CSTypeSynonymDeclaration
  -- |
  -- A kind signature declaration
  --
data CSKindDeclaration = CSKindDeclaration (Type ())
  deriving (Show, Generic, Eq)

instance Serialise CSKindDeclaration
  -- |
  -- A role declaration (name, roles)
  --
data CSRoleDeclaration = CSRoleDeclaration [Role]
  deriving (Show, Generic, Eq)

instance Serialise CSRoleDeclaration
  -- |
  -- A value declaration (name, top-level binders, optional guard, value)
  --
data CSValueDeclaration = CSValueDeclaration NameKind Int (Type ()) ToCSDB
  deriving (Show, Generic, Eq)

instance Serialise CSValueDeclaration

  -- |
  -- A foreign import declaration (name, type)
  --
data CSExternDeclaration = CSExternDeclaration ToCSDB
  deriving (Show, Generic, Eq)

instance Serialise CSExternDeclaration
  -- |
  -- A data type foreign import (name, kind)
  --
data CSExternDataDeclaration = CSExternDataDeclaration ToCSDB
  deriving (Show, Generic, Eq)

instance Serialise CSExternDataDeclaration
  -- |
  -- A fixity declaration
  --
data CSOpFixity = CSOpFixity Fixity (Qualified Ident)
  deriving (Show, Generic, Eq)

instance Serialise CSOpFixity
data CSCtorFixity = CSCtorFixity Fixity (Qualified (ProperName 'ConstructorName))
  deriving (Show, Generic, Eq)

instance Serialise CSCtorFixity
data CSTyOpFixity = CSTyOpFixity Fixity (Qualified (ProperName 'TypeName))
  deriving (Show, Generic, Eq)

instance Serialise CSTyOpFixity
  -- |
  -- A type class declaration (name, argument, implies, member declarations)
  --
data CSTypeClassDeclaration = CSTypeClassDeclaration [(Text, Maybe (Type ()))] ([Constraint ()], ToCSDB) [FunctionalDependency] [CSTypeDeclaration]
  deriving (Show, Generic, Eq)

instance Serialise CSTypeClassDeclaration

data CSTypeDeclaration = CSTypeDeclaration Ident (Type ()) ToCSDB
  deriving (Show, Generic, Eq)

instance Serialise CSTypeDeclaration
  -- |
  -- A type instance declaration (instance chain, chain index, name,
  -- dependencies, class name, instance types, member declarations)
  --
  -- The first @SourceAnn@ serves as the annotation for the entire
  -- declaration, while the second @SourceAnn@ serves as the
  -- annotation for the type class and its arguments.
data CSTypeInstanceDeclaration = CSTypeInstanceDeclaration (ChainId, Integer) ToCSDB (Qualified (ProperName 'ClassName)) ToCSDB (CSTypeInstanceBody, ToCSDB)
  deriving (Show, Generic, Eq)

instance Serialise CSTypeInstanceDeclaration

data CSTypeInstanceBody
  = CSDerivedInstance
  | CSNewtypeInstance
  | CSExplicitInstance
  deriving (Show, Eq, Generic)

instance Serialise CSTypeInstanceBody

data ToCSDB
  = ToCSDB (M.Map ModuleName ToCSDBInner)
  deriving (Show, Eq, Generic)

runToCSDB :: ToCSDB -> M.Map ModuleName ToCSDBInner
runToCSDB (ToCSDB a) = a

instance Serialise ToCSDB

instance Semigroup ToCSDB where
  ToCSDB a <> ToCSDB b = ToCSDB (M.unionWith (<>) a b)

instance Monoid ToCSDB where
  mempty = ToCSDB mempty

data ToCSDBInner
  = ToCSDBInner
    -- TODO[drathier]: all these Qualified contain SourcePos if it's referring to something in the current module. We generally don't want to store SourcePos, but since it's only local, we're already rebuilding the module at that point, so I'll leave it in.
    { _referencedCtors :: M.Map (ProperName 'ConstructorName) ()
    -- NOTE[drathier]: it's likely that we only care about the types of ctors, not types themselves, as using ctors means we care about the type shape changing, but if we just refer to the type we probably don't care what its internal structure is.
    , _referencedTypes :: M.Map (ProperName 'TypeName) ()
    , _referencedTypeOp :: M.Map (OpName 'TypeOpName) ()
    , _referencedTypeClass :: M.Map (ProperName 'ClassName) ()
    , _referencedValues :: M.Map RunIdent ()
    , _referencedValueOp :: M.Map (OpName 'ValueOpName) ()
    }
  deriving (Show, Eq, Generic)

-- NOTE[drathier]: some idents are run and re-packaged before we get here, so just matching a flat ident won't get you everything you need :( So we run the idents and wrap them up again
newtype RunIdent = RunIdent T.Text
  deriving (Show, Eq, Ord, Generic)

instance Serialise RunIdent

toRunIdent ident =
  -- TODO[drathier]: this makes me sad, what's going on here? Somehow (Ident "a") and (GenIdent (Just "a") 42) result in the same variable name in source. Why isn't the second one "$a42", like when using runIdent?
  RunIdent $
    case ident of
      GenIdent (Just name) _ -> name
      _ -> runIdent ident

-- TODO[drathier]: the type class instance decls have the same name; do they all overwrite each other in the cache? Do I need to qualify them, or skip them?

instance Serialise ToCSDBInner

instance Semigroup ToCSDBInner where
  ToCSDBInner a1 a2 a3 a4 a5 a6 <> ToCSDBInner b1 b2 b3 b4 b5 b6 = ToCSDBInner (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5) (a6 <> b6)

instance Monoid ToCSDBInner where
  mempty = ToCSDBInner mempty mempty mempty mempty mempty mempty

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

      TypeClassDictionary _ _ _ -> internalError "[drathier]: should be unreachable, all TypeClassDictionary ctors should have been expanded already"
      DeferredDictionary _ _ -> internalError "[drathier]should be unreachable, all DeferredDictionary ctors should have been expanded already"
      DerivedInstancePlaceholder _ _ -> internalError "[drathier]should be unreachable, all DerivedInstancePlaceholder ctors should have been expanded already"
      AnonymousArgument -> internalError "[drathier]: shshould be unreachable, all AnonymousArgument ctors should have been expanded already"
      Hole _ -> internalError "[drathier]: should be unreachable, all Hole ctors should have been expanded already"
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
      OpBinder _ _ -> internalError "[drathier]: should be unreachable, all OpBinder ctors should have been desugared already"
      BinaryNoParensBinder _ _ _ -> internalError "[drathier]: should be unreachable, all BinaryNoParensBinder ctors should have been desugared already"
      ParensInBinder _ -> internalError "[drathier]: should be unreachable, all ParensInBinder ctors should have been desugared already"
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
    let env = internalError "TODO[drathier]: missing env in ToCS"
    let mn = internalError "TODO[drathier]: missing mn in ToCS"
    -- TODO[drathier]: lifting CSDB values out of DB like this feels weird. Should CSDB and DB be the same type? Should we use ToCS for e.g. findDeps too?
    findDeps mn env ds
    <&> snd
    & traverse_ toCS

instance ToCS DB () where
  toCS (DB dataOrNewtypeDeclsTypeOnly dataOrNewtypeDeclsWithCtors ctorTypes typeSynonymDecls valueDecls externDecls externDataDecls opFixity ctorFixity tyOpFixity tyClassDecls tyClassInstanceDecls) = do
    dataOrNewtypeDeclsTypeOnly & traverse_ (traverse_ (\(csdb, _) -> modify (<> csdb)))
    dataOrNewtypeDeclsWithCtors & traverse_ (traverse_ (\(csdb, _) -> modify (<> csdb)))
    typeSynonymDecls & traverse_ (traverse_ (\(CSTypeSynonymDeclaration _ _ _ csdb _) -> modify (<> csdb)))
    valueDecls & traverse_ (traverse_ (\(CSValueDeclaration _ _ _ csdb) -> modify (<> csdb)))
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
csdbPutCtor =
  csdbPutHelper
   (\v refCtor ->
      v {
        _referencedCtors =
         M.insert
          refCtor
          ()
          (_referencedCtors v)
       }
   )

csdbPutType :: Qualified (ProperName 'TypeName) -> State ToCSDB ()
csdbPutType =
  csdbPutHelper
   (\v refType ->
      v {
        _referencedTypes =
         M.insert
          refType
          ()
          (_referencedTypes v)
       }
   )

csdbPutTypeOp :: Qualified (OpName 'TypeOpName) -> State ToCSDB ()
csdbPutTypeOp =
  csdbPutHelper
   (\v refTypeOp ->
      v {
        _referencedTypeOp =
         M.insert
          refTypeOp
          ()
          (_referencedTypeOp v)
       }
   )

csdbPutTypeClass :: Qualified (ProperName 'ClassName) -> State ToCSDB ()
csdbPutTypeClass =
  csdbPutHelper
   (\v refTypeClass ->
      v {
        _referencedTypeClass =
         M.insert
          refTypeClass
          ()
          (_referencedTypeClass v)
       }
   )

csdbPutIdent :: Qualified Ident -> State ToCSDB ()
csdbPutIdent =
  csdbPutHelper
   (\v ident ->
      v {
        _referencedValues =
         M.insert
          (toRunIdent ident)
          ()
          (_referencedValues v)
       }
   )

csdbPutValueOp :: Qualified (OpName 'ValueOpName) -> State ToCSDB ()
csdbPutValueOp =
  csdbPutHelper
   (\v refOp ->
      v {
        _referencedValueOp =
         M.insert
          refOp
          ()
          (_referencedValueOp v)
       }
   )


csdbPutHelper :: (ToCSDBInner -> t -> ToCSDBInner) -> Qualified t -> State ToCSDB ()
csdbPutHelper f (Qualified qBy ref) =
  modify
    (\(ToCSDB outer) ->
      ToCSDB $
      case qBy of
        BySourcePos _ -> outer
        ByModuleName mn ->
          M.alter
            (\maybeInner ->
              Just (f (maybeInner & fromMaybe mempty) ref)
            )
            mn
            outer
    )


-- DB put helpers

dbPutDataDeclaration :: ProperName 'TypeName -> ToCSDB -> CSDataDeclarationWithCtors -> State DB ()
dbPutDataDeclaration tname nctorsDB csDataDeclaration@(CSDataDeclarationWithCtors typelevel ctors) =
  modify
   (\db ->
    db
      {
        _dataOrNewtypeDeclsTypeOnly =
           M.insertWith (<>)
            tname
            [(nctorsDB, typelevel)]
            (_dataOrNewtypeDeclsTypeOnly db)
      , _dataOrNewtypeDeclsFull =
           M.insertWith (<>)
            tname
            [(nctorsDB, csDataDeclaration)]
            (_dataOrNewtypeDeclsFull db)
      , _ctorTypes =
         foldr
           (\(CSDataConstructorDeclaration ctorName _) m ->
             M.insert
              ctorName
              tname
              m
            )
            (_ctorTypes db)
            ctors
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
            (toRunIdent ident)
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
            (toRunIdent ident)
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
            (toRunIdent ident)
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
    -- TODO[drathier]: all that have a ToCSDB should have it separately like in this first row below, and the e.g. CSDataDeclaration should only contain the things that, if they change, should cause a recompile of things depending on this thing
    { _dataOrNewtypeDeclsTypeOnly :: M.Map (ProperName 'TypeName) [(ToCSDB, CSDataDeclarationTypeOnly)]
    , _dataOrNewtypeDeclsFull :: M.Map (ProperName 'TypeName) [(ToCSDB, CSDataDeclarationWithCtors)]
    , _ctorTypes :: M.Map (ProperName 'ConstructorName) (ProperName 'TypeName)
    , _typeSynonymDecls :: M.Map (ProperName 'TypeName) [CSTypeSynonymDeclaration]
    , _valueDecls :: M.Map RunIdent [CSValueDeclaration] -- TODO[drathier]: ToCSDB here too?
    , _externDecls :: M.Map RunIdent [CSExternDeclaration]
    , _externDataDecls :: M.Map (ProperName 'TypeName) [CSExternDataDeclaration]
    , _opFixity :: M.Map (OpName 'ValueOpName) [CSOpFixity]
    , _ctorFixity :: M.Map (OpName 'ValueOpName) [CSCtorFixity]
    , _tyOpFixity :: M.Map (OpName 'TypeOpName) [CSTyOpFixity]
    , _tyClassDecls :: M.Map (ProperName 'ClassName) [CSTypeClassDeclaration]
    , _tyClassInstanceDecls :: M.Map RunIdent [CSTypeInstanceDeclaration]
    }
  deriving (Show, Eq, Generic)

instance Semigroup DB where
  DB a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 <> DB b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 = DB (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5) (a6 <> b6) (a7 <> b7) (a8 <> b8) (a9 <> b9) (a10 <> b10) (a11 <> b11) (a12 <> b12)

instance Monoid DB where
  mempty = DB mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

dbToOpaque :: DB -> DBOpaque
dbToOpaque (DB a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12) =
  DBOpaque
  (a1 & M.map (\v -> v & serialise & cacheShapeHashFromByteString))
  (a2 & M.map (\v -> v & serialise & cacheShapeHashFromByteString))
  a3
  (a4 & M.map (\v -> v & serialise & cacheShapeHashFromByteString))
  (a5 & M.map (\v -> v & serialise & cacheShapeHashFromByteString))
  (a6 & M.map (\v -> v & serialise & cacheShapeHashFromByteString))
  (a7 & M.map (\v -> v & serialise & cacheShapeHashFromByteString))
  (a8 & M.map (\v -> v & serialise & cacheShapeHashFromByteString))
  (a9 & M.map (\v -> v & serialise & cacheShapeHashFromByteString))
  (a10 & M.map (\v -> v & serialise & cacheShapeHashFromByteString))
  (a11 & M.map (\v -> v & serialise & cacheShapeHashFromByteString))
  (a12 & M.map (\v -> v & serialise & cacheShapeHashFromByteString))

data DBOpaque
  = DBOpaque
    -- TODO[drathier]: ToCSDB here is only needed to figure out what we depend on. It shouldn't be needed to be stored anywhere.
    -- what things did we find?
    -- NOTE[drathier]: this gathers a list of direct dependencies, not transitive dependencies, and it doesn't fetch the shape of the dependencies. It's just the set of things we depend on, for us to fetch later.
    -- TODO[drathier]: make sure all these lists are singletons
    -- TODO[drathier]: all that have a ToCSDB should have it separately like in this first row below, and the e.g. CSDataDeclaration should only contain the things that, if they change, should cause a recompile of things depending on this thing
    { _dataOrNewtypeDeclsTypeOnly_opaque :: M.Map (ProperName 'TypeName) CacheShapeHash
    , _dataOrNewtypeDeclsFull_opaque :: M.Map (ProperName 'TypeName) CacheShapeHash
    , _ctorTypes_opaque :: M.Map (ProperName 'ConstructorName) (ProperName 'TypeName)
    , _typeSynonymDecls_opaque :: M.Map (ProperName 'TypeName) CacheShapeHash
    , _valueDecls_opaque :: M.Map RunIdent CacheShapeHash
    , _externDecls_opaque :: M.Map RunIdent CacheShapeHash
    , _externDataDecls_opaque :: M.Map (ProperName 'TypeName) CacheShapeHash
    , _opFixity_opaque :: M.Map (OpName 'ValueOpName) CacheShapeHash
    , _ctorFixity_opaque :: M.Map (OpName 'ValueOpName) CacheShapeHash
    , _tyOpFixity_opaque :: M.Map (OpName 'TypeOpName) CacheShapeHash
    , _tyClassDecls_opaque :: M.Map (ProperName 'ClassName) CacheShapeHash
    , _tyClassInstanceDecls_opaque :: M.Map RunIdent CacheShapeHash
    }
  deriving (Show, Eq, Generic)

instance Serialise DBOpaque

instance Semigroup DBOpaque where
  DBOpaque a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 <> DBOpaque b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 = DBOpaque (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5) (a6 <> b6) (a7 <> b7) (a8 <> b8) (a9 <> b9) (a10 <> b10) (a11 <> b11) (a12 <> b12)

instance Monoid DBOpaque where
  mempty = DBOpaque mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

cacheShapeHashFromByteString :: ByteString -> CacheShapeHash
cacheShapeHashFromByteString b =
  let
      digest :: Hash.Digest Hash.SHA256
      digest = b & Hash.hashlazy
  in
    digest & show & BS8.fromString & CacheShapeHash

newtype CacheShapeHash = CacheShapeHash BS8.ByteString
  deriving (Show, Eq, Generic)

instance Serialise CacheShapeHash

dbIsctExports :: M.Map ModuleName DB -> ExportSummary -> DB -> DB
dbIsctExports upstreamDBs (ExportSummary _ typeName typeOpName typeClass typeClassInstance valueOpName reExportedRefs) (DB a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12) =
  let
    upstreamReExports =
      M.intersectionWith
        (\innerExportSummary innerDB -> dbIsctExports upstreamDBs innerExportSummary innerDB)
        reExportedRefs
        upstreamDBs
    ourDB =
      DB
        (M.intersectionWith (\_ b -> b) typeName a1)
        a2
        a3
        (M.intersectionWith (\_ b -> b) typeName a4)
        a5
        a6
        (M.intersectionWith (\_ b -> b) typeName a7)
        (M.intersectionWith (\_ b -> b) valueOpName a8)
        (M.intersectionWith (\_ b -> b) valueOpName a9)
        (M.intersectionWith (\_ b -> b) typeOpName a10)
        (M.intersectionWith (\_ b -> b) typeClass a11)
        (M.intersectionWith (\_ b -> b) typeClassInstance a12)
  in
  foldl'
    (\dbSoFar upstreamDB ->
      -- [drathier]: <> is Map union; it keeps left arg on conflict
      dbSoFar <> upstreamDB
    )
    ourDB
    upstreamReExports

dbOpaqueIsctExports :: M.Map ModuleName DBOpaque -> ExportSummary -> DBOpaque -> DBOpaque
dbOpaqueIsctExports upstreamDBs (ExportSummary _ typeName typeOpName typeClass typeClassInstance valueOpName reExportedRefs) (DBOpaque a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12) =
  let
    upstreamReExports =
      M.intersectionWith
        (\innerExportSummary innerDB -> dbOpaqueIsctExports upstreamDBs innerExportSummary innerDB)
        reExportedRefs
        upstreamDBs
    ourDB =
      DBOpaque
        (M.intersectionWith (\_ b -> b) typeName a1)
        a2
        a3
        (M.intersectionWith (\_ b -> b) typeName a4)
        a5
        a6
        (M.intersectionWith (\_ b -> b) typeName a7)
        (M.intersectionWith (\_ b -> b) valueOpName a8)
        (M.intersectionWith (\_ b -> b) valueOpName a9)
        (M.intersectionWith (\_ b -> b) typeOpName a10)
        (M.intersectionWith (\_ b -> b) typeClass a11)
        (M.intersectionWith (\_ b -> b) typeClassInstance a12)
  in
  foldl'
    (\dbSoFar upstreamDB ->
      -- [drathier]: <> is Map union; it keeps left arg on conflict
      dbSoFar <> upstreamDB
    )
    ourDB
    upstreamReExports


dbDiffDiff (DB a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12) (DB b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12) =
    DB
      ((M.differenceWith (\x y -> if x == y then Nothing else Just x) a1 b1))
      ((M.differenceWith (\x y -> if x == y then Nothing else Just x) a2 b2))
      ((M.differenceWith (\x y -> if x == y then Nothing else Just x) a3 b3))
      ((M.differenceWith (\x y -> if x == y then Nothing else Just x) a4 b4))
      ((M.differenceWith (\x y -> if x == y then Nothing else Just x) a5 b5))
      ((M.differenceWith (\x y -> if x == y then Nothing else Just x) a6 b6))
      ((M.differenceWith (\x y -> if x == y then Nothing else Just x) a7 b7))
      ((M.differenceWith (\x y -> if x == y then Nothing else Just x) a8 b8))
      ((M.differenceWith (\x y -> if x == y then Nothing else Just x) a9 b9))
      ((M.differenceWith (\x y -> if x == y then Nothing else Just x) a10 b10))
      ((M.differenceWith (\x y -> if x == y then Nothing else Just x) a11 b11))
      ((M.differenceWith (\x y -> if x == y then Nothing else Just x) a12 b12))

dbOpaqueDiffDiff (DBOpaque a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12) (DBOpaque b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12) =
    DBOpaque
      ((M.differenceWith (\x y -> if x == y then Nothing else Just x) a1 b1))
      ((M.differenceWith (\x y -> if x == y then Nothing else Just x) a2 b2))
      ((M.differenceWith (\x y -> if x == y then Nothing else Just x) a3 b3))
      ((M.differenceWith (\x y -> if x == y then Nothing else Just x) a4 b4))
      ((M.differenceWith (\x y -> if x == y then Nothing else Just x) a5 b5))
      ((M.differenceWith (\x y -> if x == y then Nothing else Just x) a6 b6))
      ((M.differenceWith (\x y -> if x == y then Nothing else Just x) a7 b7))
      ((M.differenceWith (\x y -> if x == y then Nothing else Just x) a8 b8))
      ((M.differenceWith (\x y -> if x == y then Nothing else Just x) a9 b9))
      ((M.differenceWith (\x y -> if x == y then Nothing else Just x) a10 b10))
      ((M.differenceWith (\x y -> if x == y then Nothing else Just x) a11 b11))
      ((M.differenceWith (\x y -> if x == y then Nothing else Just x) a12 b12))


findDeps :: ModuleName -> Environment -> [Declaration] -> [(Declaration, DB)]
findDeps mn env ds =
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
      (a, mempty & execState (findDepsImpl getKind getRole mn env a))
     )
    & filter (\(_, db) -> db /= mempty)

findDepsImpl
  :: (KindSignatureFor -> ProperName 'TypeName -> Maybe CSKindDeclaration)
  -> (ProperName 'TypeName -> Maybe CSRoleDeclaration)
  -> ModuleName
  -> Environment
  -> Declaration
  -> State DB ()
findDepsImpl getKind getRole mn env d =
  -- data Declaration
  case d of
    -- DataDeclaration SourceAnn DataDeclType (ProperName 'TypeName) [(Text, Maybe SourceType)] [DataConstructorDeclaration]
    DataDeclaration _ dataOrNewtype tname targs ctors -> do
      let (nctorsValue, nctorsDB) = mempty & runState (traverse toCS ctors)

      let mkind =
            case dataOrNewtype of
              Data -> getKind DataSig tname
              Newtype -> getKind NewtypeSig tname

      dbPutDataDeclaration tname nctorsDB
        (CSDataDeclarationWithCtors
          (CSDataDeclarationTypeOnly dataOrNewtype tname targs mkind (getRole tname))
          nctorsValue
        )
      -- pure $ f (show ("DataDeclaration", tname)) $
      --   show ("DataDeclaration", dataOrNewtype, tname, targs, ctors)
    -- DataBindingGroupDeclaration (NEL.NonEmpty Declaration)
    DataBindingGroupDeclaration decls ->
      -- rarely used here, but used by e.g. Data.Void
      traverse_ (findDepsImpl getKind getRole mn env) decls

    -- TypeSynonymDeclaration SourceAnn (ProperName 'TypeName) [(Text, Maybe SourceType)] SourceType
    TypeSynonymDeclaration _ tname targs stype -> do
      -- TODO[drathier]: KindedType.purs has a "Just SourceType" targ. I don't know how to handle it here. Right now I'm just storing it as-is.
      let nstype = stype <&> const ()
      let ntargs = targs <&> fmap (fmap (fmap (const ())))
      let nstypeDB = stype & replaceTypeSynonyms (types env) (typeSynonyms env <&> snd) & flip execState mempty
      dbPutTypeSynonymDeclaration tname (CSTypeSynonymDeclaration tname ntargs nstype nstypeDB (getKind TypeSynonymSig tname))

    -- KindDeclaration SourceAnn KindSignatureFor (ProperName 'TypeName) SourceType
    KindDeclaration _ _ _ _ ->
      internalError "[drathier]: should be unreachable, all KindDeclaration ctors should have been filtered out earlier"

    -- RoleDeclaration {-# UNPACK #-} !RoleDeclarationData
    RoleDeclaration _ ->
      -- ASSUMPTION[drathier]: got this compiler error when testing, assuming it to be true forever "Role declarations are only supported for data types, not for type synonyms nor type classes." We'll likely incorrectly  cache things wrt this if this changes in the future. Testing also shows that it works fine for newtypes, so I'm supporting that.
      internalError "[drathier]: should be unreachable, all RoleDeclaration ctors should have been filtered out earlier"
    -- TypeDeclaration {-# UNPACK #-} !TypeDeclarationData
    TypeDeclaration _ ->
      internalError "ASSUMPTION[drathier]: should be unreachable, all TypeDeclaration ctors should have been extracted earlier"
    -- ValueDeclaration {-# UNPACK #-} !(ValueDeclarationData [GuardedExpr])
    ValueDeclaration (ValueDeclarationData _ ident namekind binders exprs) -> do
      -- TODO[drathier]: do we really need expr in here too? Yes, we need to know what modules its value and type refers to at least.
      let !(_, nexprDB) = mempty & runState (traverse_ toCS exprs)
      let tipe = case M.lookup (Qualified (ByModuleName mn) ident) (names env) of
                    Nothing -> internalError "drathier1"
                    Just (ty, _, _) -> const () <$> ty
      dbPutValueDeclaration ident (CSValueDeclaration namekind (length binders) tipe nexprDB)

    -- BoundValueDeclaration SourceAnn Binder Expr
    BoundValueDeclaration _ _ _ ->
      internalError "ASSUMPTION[drathier]: should be unreachable, all BoundValueDeclaration ctors should have been desugared earlier"
    -- BindingGroupDeclaration (NEL.NonEmpty ((SourceAnn, Ident), NameKind, Expr))
    BindingGroupDeclaration decls ->
      -- rarely used here, but used by e.g. instance HeytingAlgebra Boolean, since its type class function implementations call eachother (implies calls not)
      decls
        <&> (\((sourceAnn, ident), nameKind, expr) ->
          ValueDeclaration (ValueDeclarationData sourceAnn ident nameKind [] [GuardedExpr [] expr])
        )
        & traverse_ (findDepsImpl getKind getRole mn env)

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
    ImportDeclaration _ modu importDeclType mAlias -> pure ()
    -- TypeClassDeclaration SourceAnn (ProperName 'ClassName) [(Text, Maybe SourceType)] [SourceConstraint] [FunctionalDependency] [Declaration]
    TypeClassDeclaration _ className targs constraints fnDeps decls -> do
      ndecls <- decls & traverse (\decl ->
        case decl of
          TypeDeclaration (TypeDeclarationData _ ident tipe) -> do
            let (ntipe, ntipeDB) = mempty & runState (toCS tipe)
            pure $ CSTypeDeclaration ident (const () <$> tipe) ntipeDB
          v -> error ("ASSUMPTION[drathier]: The inner declarations in the type class declaration are just TypeDeclarations." ++ show v)
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
              Left _ -> internalError "ASSUMPTION[drathier]: we'll never get a Text value here; even generated instances have Idents"
              Right v -> v
      dbPutTypeInstanceDeclaration instanceName (CSTypeInstanceDeclaration (chainId, chainIdIndex) ndependencySourceConstraintsDB className ninstanceSourceTypesDB (nderivedNewtypeExplicitNoDecls, nderivedNewtypeExplicit))


replaceTypeSynonyms
  :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
  -> M.Map (Qualified (ProperName 'TypeName)) (Type a) -> Type a -> State ToCSDB (Type a)
replaceTypeSynonyms typesMap typeSynonymsMap =
  -- NOTE[drathier]: replaceAllTypeSynonyms exists, but I couldn't get it to work in this context.
  -- TODO[drathier]: this shouldn't have to look further than the module we imported the type alias from. Currently it fetches all the way down, because it looks at the Environment, rather than the Externs cache shape. On the other hand, it's unlikely to matter much in practice.
  let f t =
        case t of
          TypeConstructor _ qt@(Qualified (ByModuleName modu) tipe) ->
            -- type aliases (type synonyms)
            case M.lookup qt typeSynonymsMap of
              Just v -> do
                csdbPutType qt
                replaceTypeSynonyms typesMap typeSynonymsMap v
              Nothing | moduIsPrim modu -> csdbPutType qt >> pure t
              Nothing | M.member qt typesMap -> csdbPutType qt >> pure t
              Nothing -> internalError (sShow ("[drathier]: couldn't find upstream type constructor in env", qt, modu, tipe))
          _ -> pure t
  in everywhereOnTypesM f

-- TODO[drathier]: this is very similar to ToCSDBInner, but not quite. This tracks exported values, ToCSDBInner tracks used types.
data ExportSummary =
  ExportSummary
    { _refValue :: M.Map Ident ()
    , _refTypeName :: M.Map (ProperName 'TypeName) ()
    , _refTypeOpName :: M.Map (OpName 'TypeOpName) ()
    , _refTypeClass :: M.Map (ProperName 'ClassName) ()
    , _refTypeClassInstance :: M.Map RunIdent ()
    , _refOpName :: M.Map (OpName 'ValueOpName) ()
    -- [drathier]: re-exports of whole modules are desugared to one-by-one export refs, so we don't have to handle them here
    , _reExportRef :: M.Map ModuleName ExportSummary
    } deriving (Show, Eq)

instance Monoid ExportSummary where
  mempty = ExportSummary mempty mempty mempty mempty mempty mempty mempty

instance Semigroup ExportSummary where
  ExportSummary a1 a2 a3 a4 a5 a6 a7 <> ExportSummary b1 b2 b3 b4 b5 b6 b7 = ExportSummary (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5) (a6 <> b6) (a7 <> b7)

exsumPutTypeOpName ref v =
  v {
    _refTypeOpName =
     M.insert
      ref
      ()
      (_refTypeOpName v)
   }

exsumPutTypeName ref v =
  v {
    _refTypeName =
     M.insert
      ref
      ()
      (_refTypeName v)
   }

exsumPutValue ref v =
  v {
    _refValue =
     M.insert
      ref
      ()
      (_refValue v)
   }

exsumPutOpName ref v =
  v {
    _refOpName =
     M.insert
      ref
      ()
      (_refOpName v)
   }

exsumPutExportRef (ExportSource _ origSrc) ref v =
  v {
    _reExportRef =
     M.insertWith
      (<>)
      origSrc
      ref
      (_reExportRef v)
   }

exsumPutTypeClassRef ref v =
  v {
    _refTypeClass =
     M.insert
      ref
      ()
      (_refTypeClass v)
   }

exsumPutTypeClassInstance ref v =
  v {
    _refTypeClassInstance =
     M.insert
      ref
      ()
      (_refTypeClassInstance v)
   }


findExportedThings :: [DeclarationRef] -> ExportSummary
findExportedThings declRefs =
  foldl findExportedThingsImpl mempty declRefs

findExportedThingsImpl :: ExportSummary -> DeclarationRef -> ExportSummary
findExportedThingsImpl exsum declRef =
  case declRef of
    TypeClassRef _ className -> exsumPutTypeClassRef className exsum
    TypeOpRef _ tyOpName -> exsumPutTypeOpName tyOpName exsum
    TypeRef _ tyName mCtorNames -> exsumPutTypeName tyName exsum
    ValueRef _ ident -> exsumPutValue ident exsum
    ValueOpRef _ opName -> exsumPutOpName opName exsum
    TypeInstanceRef _ ident _ -> exsumPutTypeClassInstance (toRunIdent ident) exsum
    ReExportRef _ src ref -> exsumPutExportRef src (findExportedThings [ref]) exsum
    ModuleRef _ modu ->
      -- [drathier]: re-exports of whole modules are desugared to one-by-one export refs, so we don't have to handle them here. However, they're still left in, so we have to ignore them here, rather than assert that we never see any value like this here
      exsum

-- | Generate an externs file for all declarations in a module.
--
-- The `Map Ident Ident` argument should contain any top-level `GenIdent`s that
-- were rewritten to `Ident`s when the module was compiled; this rewrite only
-- happens in the CoreFn, not the original module AST, so it needs to be
-- applied to the exported names here also. (The appropriate map is returned by
-- `L.P.Renamer.renameInModule`.)
moduleToExternsFile :: M.Map ModuleName DBOpaque -> Module -> Environment -> M.Map Ident Ident -> ExternsFile
moduleToExternsFile _ (Module _ _ _ _ Nothing) _ _ = internalError "moduleToExternsFile: module exports were not elaborated"
-- data Module = Module SourceSpan [Comment] ModuleName [Declaration] (Maybe [DeclarationRef])
moduleToExternsFile upstreamDBs (Module ss _ mn ds (Just exps)) env renamedIdents =
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
  let possiblyImportedTypeAliasesFrom :: M.Map ModuleName () -- [(ProperName 'TypeName)]
      possiblyImportedTypeAliasesFrom =
        ds
        & concatMap (\case
          -- TODO[drathier]: look at importDeclType
          ImportDeclaration _ modu _importDeclType _mAlias ->
            [modu]
          _ -> []
        )
        <&> (,())
        & M.fromList
  in
  let !exportedThings = findExportedThings exps in
  -- let !importedThings = findImportedThings exps in
  let findDepsRes = findDeps mn env ds in
  let dbDeps = foldl (<>) mempty (snd <$> findDepsRes) in
  let csdbDeps = flip execState mempty $ toCS $ dbDeps in
  let efOurCacheShapes = dbDeps & dbToOpaque & dbOpaqueIsctExports upstreamDBs exportedThings in
  -- let !_ = trace (sShow ("###moduleToExternsFile findExportedThings", mn, exportedThings)) () in
{-
  let !_ = trace (show ("###moduleToExternsFile mn", mn)) () in
  let !_ = trace (show ("###moduleToExternsFile ds.BoundValueDeclaration", M.lookup "BoundValueDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile ds.BindingGroupDeclaration", M.lookup "BindingGroupDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile ds.ExternDeclaration", M.lookup "ExternDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile ds.ExternDataDeclaration", M.lookup "ExternDataDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile ds.FixityDeclaration", M.lookup "FixityDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile ds.ImportDeclaration", M.lookup "ImportDeclaration" sds)) () in
-}
  -- let !_ = trace (sShow ("###moduleToExternsFile ds.TypeClassDeclaration", M.lookup "TypeClassDeclaration" sds)) () in
  -- let !_ = trace (sShow ("###moduleToExternsFile ds.TypeInstanceDeclaration", M.lookup "TypeInstanceDeclaration" sds)) () in
{-
  let !_ = trace (show ("###moduleToExternsFile ds.DataDeclaration", M.lookup "DataDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile ds.DataBindingGroupDeclaration", M.lookup "DataBindingGroupDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile ds.TypeSynonymDeclaration", M.lookup "TypeSynonymDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile ds.KindDeclaration", M.lookup "KindDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile ds.RoleDeclaration", M.lookup "RoleDeclaration" sds)) () in
  let !_ = trace (show ("###moduleToExternsFile ds.TypeDeclaration", M.lookup "TypeDeclaration" sds)) () in
-}
  -- let !_ = trace (sShow ("###moduleToExternsFile ds.ValueDeclaration", M.lookup "ValueDeclaration" sds)) () in
{-
  let !_ = trace (show ("###moduleToExternsFile exps", exps)) () in
  let !_ = trace (show ("###moduleToExternsFile renamedIdents", renamedIdents)) () in
  let !_ = trace (show ("-------")) () in
  let !_ = trace (show ("###moduleToExternsFile findDeps", findDepsRes)) () in
  let !_ = trace (show ("###moduleToExternsFile findDepsDB", dbDeps)) () in -- NOTE[drathier]: this is the beginning of the CacheShape data; i.e. what's the shape of this data, according to anyone who wants to use it
  let !_ = trace (show ("###moduleToExternsFile findDepsCSDB", csdbDeps)) () in -- NOTE[drathier]: this will become the list of things we depend on from other modules, i.e. the things we need to look up, copy in, and diff against the old values to figure out if we should recompile or not
-}

  -- ("WARNING: old is empty",ModuleName "B")
  -- TODO[drathier]: type aliases aren't tracked across deps yet; same bug as with the old attempt. Solved by re-exporting the relevant info. It seems like no expr has the type alias type anywhere; maybe type aliases are always lost? They're available in the ast at least, so we can get at them, even if we have to assume everyone depends on all type aliases always, and possibly same for type classes


  -- TODO[drathier]: handle imports? we only look at what's actually used, so what's the point?
  -- TODO[drathier]: handle exports?
  -- TODO[drathier]: handle re-exports?

  let efUpstreamCacheShapes :: M.Map ModuleName DBOpaque
      efUpstreamCacheShapes =
          let currentDeps :: M.Map ModuleName ToCSDBInner
              currentDeps =
                runToCSDB csdbDeps
                -- TODO[drathier]: we always depend on all imported type aliases. We shouldn't have to do that.
                & M.merge
                  (M.mapMissing (\_ () -> mempty))
                  (M.mapMissing (\_ b -> b))
                  (M.zipWithMatched (\_ () b -> b))
                  possiblyImportedTypeAliasesFrom

                -- don't look for ourselves or built-in modules in the upstream cache
                & M.delete mn
                & M.filterWithKey (\m _ -> moduIsPrim m == False)
          in
          M.merge
            M.dropMissing
            (M.mapMissing (\k a -> error ("[drathier]: cache key only in currentDeps2, missing in build history: " <> show
              ( "key", k
              , "modu", mn
              , "commonKeys", M.keys (M.intersection (const () <$> currentDeps) (const () <$> upstreamDBs))
              , "currentDepsKeys", M.keys currentDeps
              , "v", a
              ))))
            (M.zipWithMatched (\_mnDep up (ToCSDBInner
                ctors
                types
                typeOp
                typeClasses
                values
                valueOp) ->

                let
                    dbCtorToType :: M.Map (ProperName 'TypeName) (ProperName 'ConstructorName)
                    dbCtorToType =
                      _ctorTypes_opaque up
                      & M.toList
                      <&> (\(a,b) -> (b,a))
                      & M.fromList

                    typesRefByCtors :: M.Map (ProperName 'TypeName) ()
                    typesRefByCtors =
                      ctors
                        & M.intersectionWith (\a _ -> a) (_ctorTypes_opaque up)
                        & M.elems
                        <&> (,())
                        & M.fromList
                in
              -- TODO[drathier]: it would be nice to check that each of the names we tried to look up matched exactly one thing when building this DB

              -- TODO[drathier]: the dry run envvar should still run the caching logic, to see if it would've skipped the recompile, and also compare the rebuilt exts to the cached ones, to see if the rebuild was needed. If there's a mismatch in either direction, print it to stdout so I can debug it later.

              DBOpaque
                (M.intersectionWith (\_ b -> b) types (_dataOrNewtypeDeclsTypeOnly_opaque up))
                (M.intersectionWith (\_ b -> b) typesRefByCtors (_dataOrNewtypeDeclsFull_opaque up))
                (M.intersectionWith (\_ b -> b) ctors (_ctorTypes_opaque up))
                (_typeSynonymDecls_opaque up) -- (M.intersectionWith (\_ b -> b) types (_typeSynonymDecls up)) -- TODO[drathier]: We don't really know if a type alias was used or not, because type aliases get replaced with the thing they're aliasing before we get here. However, we could look at explicit exports and explicit imports to filter this a bit.
                (M.intersectionWith (\_ b -> b) values (_valueDecls_opaque up))
                (M.intersectionWith (\_ b -> b) values (_externDecls_opaque up))
                (M.intersectionWith (\_ b -> b) types (_externDataDecls_opaque up))
                (M.intersectionWith (\_ b -> b) valueOp (_opFixity_opaque up))
                (M.intersectionWith (\_ b -> b) valueOp (_ctorFixity_opaque up))
                (M.intersectionWith (\_ b -> b) typeOp (_tyOpFixity_opaque up))
                (M.intersectionWith (\_ b -> b) typeClasses (_tyClassDecls_opaque up))
                (M.intersectionWith (\_ b -> b) values (_tyClassInstanceDecls_opaque up))
        ))
        upstreamDBs
        currentDeps
        & M.filter (/= (mempty :: DBOpaque))

  in
  -- let !_ = trace (sShow ("###moduleToExternsFile efUpstreamCacheShapes", mn, efUpstreamCacheShapes)) () in
  -- let !_ = trace (sShow ("###moduleToExternsFile efOurCacheShapes", mn, efOurCacheShapes)) () in
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

moduIsPrim :: ModuleName -> Bool
moduIsPrim (ModuleName n) = "Prim" `T.isPrefixOf` n
