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
data CSDataDeclaration = CSDataDeclaration DataDeclType (ProperName 'TypeName) [(Text, Maybe SourceType)] [CSDataConstructorDeclaration]
  deriving (Show)
  -- |
  -- A minimal mutually recursive set of data type declarations
  --
data CSDataBindingGroupDeclaration = CSDataBindingGroupDeclaration (NEL.NonEmpty Declaration)
  deriving (Show)
  -- |
  -- A type synonym declaration (name, arguments, type)
  --
data CSTypeSynonymDeclaration = CSTypeSynonymDeclaration (ProperName 'TypeName) [(Text, Maybe SourceType)] SourceType
  deriving (Show)
  -- |
  -- A kind signature declaration
  --
data CSKindDeclaration = CSKindDeclaration KindSignatureFor (ProperName 'TypeName) SourceType
  deriving (Show)
  -- |
  -- A role declaration (name, roles)
  --
data CSRoleDeclaration = CSRoleDeclaration {-# UNPACK #-} !RoleDeclarationData
  deriving (Show)
  -- |
  -- A type declaration for a value (name, ty)
  --
data CSTypeDeclaration = CSTypeDeclaration {-# UNPACK #-} !TypeDeclarationData
  deriving (Show)
  -- |
  -- A value declaration (name, top-level binders, optional guard, value)
  --
data CSValueDeclaration = CSValueDeclaration {-# UNPACK #-} !(ValueDeclarationData [GuardedExpr])
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
data CSExternDeclaration = CSExternDeclaration Ident SourceType
  deriving (Show)
  -- |
  -- A data type foreign import (name, kind)
  --
data CSExternDataDeclaration = CSExternDataDeclaration (ProperName 'TypeName) SourceType
  deriving (Show)
  -- |
  -- A fixity declaration
  --
data CSFixityDeclaration = CSFixityDeclaration (Either ValueFixity TypeFixity)
  deriving (Show)
  -- |
  -- A module import (module name, qualified/unqualified/hiding, optional "qualified as" name)
  --
data CSImportDeclaration = CSImportDeclaration ModuleName ImportDeclarationType (Maybe ModuleName)
  deriving (Show)
  -- |
  -- A type class declaration (name, argument, implies, member declarations)
  --
data CSTypeClassDeclaration = CSTypeClassDeclaration (ProperName 'ClassName) [(Text, Maybe SourceType)] [SourceConstraint] [FunctionalDependency] [Declaration]
  deriving (Show)
  -- |
  -- A type instance declaration (instance chain, chain index, name,
  -- dependencies, class name, instance types, member declarations)
  --
  -- The first @SourceAnn@ serves as the annotation for the entire
  -- declaration, while the second @SourceAnn@ serves as the
  -- annotation for the type class and its arguments.
data CSTypeInstanceDeclaration = CSTypeInstanceDeclaration ChainId Integer (Either Text Ident) [SourceConstraint] (Qualified (ProperName 'ClassName)) [SourceType] TypeInstanceBody
  deriving (Show)

  -- TODO[drathier]: fix this hack
instance Eq CSDataDeclaration where
    a == b = show a == show b
instance Eq CSDataBindingGroupDeclaration where
    a == b = show a == show b
instance Eq CSTypeSynonymDeclaration where
    a == b = show a == show b
instance Eq CSKindDeclaration where
    a == b = show a == show b
instance Eq CSRoleDeclaration where
    a == b = show a == show b
instance Eq CSTypeDeclaration where
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



data CSDataConstructorDeclaration
  = CSDataConstructorDeclaration
    { csdataCtorName :: !(ProperName 'ConstructorName)
    , csdataCtorFields :: ![(Ident, Type ())]
    }
  deriving (Show, Eq)

data ToCSDB
  = ToCSDB
    { _referencedCtors :: M.Map (Qualified (ProperName 'TypeName)) ()
    , _referencedTypeOp :: M.Map (Qualified (OpName 'TypeOpName)) ()
    , _referencedTypeClass :: M.Map (Qualified (ProperName 'ClassName)) ()
    }
  deriving (Show, Eq)


instance Semigroup ToCSDB where
  ToCSDB a1 a2 a3 <> ToCSDB b1 b2 b3 = ToCSDB (a1 <> b1) (a2 <> b2) (a3 <> b3)

instance Monoid ToCSDB where
  mempty = ToCSDB mempty mempty mempty


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
                storeTypeRefs typeWithSrcAnn
                pure (ident, t2)
            )
      pure $
        CSDataConstructorDeclaration
          ctorName
          ctorFields2


storeConstraintTypes :: Constraint a -> State ToCSDB ()
storeConstraintTypes (Constraint _ refTypeClass kindArgs targs mdata) = do
  csdbPutTypeClass refTypeClass
  traverse_ storeTypeRefs kindArgs
  traverse_ storeTypeRefs targs

-- CSDB put helpers

csdbPutCtor :: Qualified (ProperName 'TypeName) -> State ToCSDB ()
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


storeTypeRefs :: Type a -> State ToCSDB ()
storeTypeRefs t =
  case t of
    TUnknown _ _ -> pure ()
    TypeVar _ _ -> pure ()
    TypeLevelString _ _ -> pure ()
    TypeLevelInt _ _ -> pure ()
    TypeWildcard _ _ -> pure ()
    TypeConstructor _ refCtor -> do
      csdbPutCtor refCtor

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
    {_dataOrNewtypeDecls :: M.Map (ProperName 'TypeName) [(ToCSDB, CSDataDeclaration)]
    }
  deriving (Show, Eq)

instance Semigroup DB where
  DB a1 <> DB b1 = DB (a1 <> b1)

instance Monoid DB where
  mempty = DB mempty



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
  fmap (\a -> do
    (a, mempty & execState (findDepsImpl a))
   )
   ds
   & filter (\(_, db) -> db /= mempty)

findDepsImpl :: Declaration -> State DB ()
findDepsImpl d =
  -- data Declaration
  case d of
    -- DataDeclaration SourceAnn DataDeclType (ProperName 'TypeName) [(Text, Maybe SourceType)] [DataConstructorDeclaration]
    DataDeclaration _ dataOrNewtype tname targs ctors -> do
      db <- get

      let (nctorsValue, nctorsDB) = mempty & runState (traverse toCS ctors)

      put (
        db
          {
            _dataOrNewtypeDecls =
               M.insertWith (<>)
                tname
                [(nctorsDB, CSDataDeclaration dataOrNewtype tname targs nctorsValue)]
                (_dataOrNewtypeDecls db)
          }
       )
      -- pure $ f (show ("DataDeclaration", tname)) $
      --   show ("DataDeclaration", dataOrNewtype, tname, targs, ctors)
    -- DataBindingGroupDeclaration (NEL.NonEmpty Declaration)
    DataBindingGroupDeclaration _ -> pure ()
    -- TypeSynonymDeclaration SourceAnn (ProperName 'TypeName) [(Text, Maybe SourceType)] SourceType
    TypeSynonymDeclaration _ _ _ _ -> pure ()
    -- KindDeclaration SourceAnn KindSignatureFor (ProperName 'TypeName) SourceType
    KindDeclaration _ _ _ _ -> pure ()
    -- RoleDeclaration {-# UNPACK #-} !RoleDeclarationData
    RoleDeclaration _ -> pure ()
    -- TypeDeclaration {-# UNPACK #-} !TypeDeclarationData
    TypeDeclaration _ -> pure ()
    -- ValueDeclaration {-# UNPACK #-} !(ValueDeclarationData [GuardedExpr])
    ValueDeclaration _ -> pure ()
    -- BoundValueDeclaration SourceAnn Binder Expr
    BoundValueDeclaration _ _ _ -> pure ()
    -- BindingGroupDeclaration (NEL.NonEmpty ((SourceAnn, Ident), NameKind, Expr))
    BindingGroupDeclaration _ -> pure ()
    -- ExternDeclaration SourceAnn Ident SourceType
    ExternDeclaration _ _ _ -> pure ()
    -- ExternDataDeclaration SourceAnn (ProperName 'TypeName) SourceType
    ExternDataDeclaration _ _ _ -> pure ()
    -- FixityDeclaration SourceAnn (Either ValueFixity TypeFixity)
    FixityDeclaration _ _ -> pure ()
    -- ImportDeclaration SourceAnn ModuleName ImportDeclarationType (Maybe ModuleName)
    ImportDeclaration _ _ _ _ -> pure ()
    -- TypeClassDeclaration SourceAnn (ProperName 'ClassName) [(Text, Maybe SourceType)] [SourceConstraint] [FunctionalDependency] [Declaration]
    TypeClassDeclaration _ _ _ _ _ _ -> pure ()
    TypeInstanceDeclaration _ _ _ _ _ _ _ _ _ -> pure ()


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
