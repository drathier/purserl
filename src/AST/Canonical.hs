{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Canonical
--  ( Expr, Expr_(..)
--  , CaseBranch(..)
--  , FieldUpdate(..)
--  , CtorOpts(..)
--  -- definitions
--  , Def(..)
--  , Decls(..)
--  -- patterns
--  , Pattern, Pattern_(..)
--  , PatternCtorArg(..)
--  -- types
--  , Annotation(..)
--  , Type(..)
--  , AliasType(..)
--  , FieldType(..)
--  , fieldsToList
--  -- modules
--  , Module(..)
--  , Alias(..)
--  , Binop(..)
--  , Union(..)
--  , Ctor(..)
--  , Exports(..)
--  , Export(..)
--  , Effects(..)
--  , Port(..)
--  , Manager(..)
--  )
  (Annotation(..)
  , Type(..)
  , AliasType(..)
  , FieldType(..)
  , fieldsToList
  )
  where

{- Creating a canonical AST means finding the home module for all variables.
So if you have L.map, you need to figure out that it is from the elm/core
package in the List module.

In later phases (e.g. type inference, exhaustiveness checking, optimization)
you need to look up additional info from these modules. What is the type?
What are the alternative type constructors? These lookups can be quite costly,
especially in type inference. To reduce costs the canonicalization phase
caches info needed in later phases. This means we no longer build large
dictionaries of metadata with O(log(n)) lookups in those phases. Instead
there is an O(1) read of an existing field! I have tried to mark all
cached data with comments like:

-- CACHE for exhaustiveness
-- CACHE for inference

So it is clear why the data is kept around.
-}

import Prelude
import Data.Word (Word16)
--
--import Control.Monad (liftM, liftM2, liftM3, liftM4, liftM5, replicateM)
--import Data.Binary
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Name (Name)
--
--import qualified AST.Source as Src
--import qualified AST.Utils.Binop as Binop
--import qualified AST.Utils.Shader as Shader
-- import qualified Data.Index as Index
--import qualified Elm.Float as EF
import qualified Elm.ModuleName as ModuleName
--import qualified Elm.String as ES
-- import qualified Reporting.Annotation as A
--
-- import qualified Data.Name as Name
-- import qualified Data.Utf8 as Utf8
--import qualified Parse.Primitives as P
--import qualified Parse.Space as Space
--import qualified Reporting.Error.Syntax as E
--import qualified Data.ByteString.Builder as B
--import qualified Data.ByteString.Lazy as BSL
--import qualified Data.ByteString as BS
--
--
---- EXPRESSIONS
--
--
--type Expr =
--  A.Located Expr_
--
--
---- CACHE Annotations for type inference
--data Expr_
--  = VarLocal Name
--  | VarTopLevel ModuleName.Canonical Name
--  | VarKernel Name Name
--  | VarForeign ModuleName.Canonical Name Annotation
--  | VarCtor CtorOpts ModuleName.Canonical Name Index.ZeroBased Annotation
--  | VarDebug ModuleName.Canonical Name Annotation
--  | VarOperator Name ModuleName.Canonical Name Annotation -- CACHE real name for optimization
--  | Chr ES.String
--  | Str ES.String
--  | Int Int
--  | Float EF.Float
--  | List [Expr]
--  | Negate Expr
--  | Binop Name ModuleName.Canonical Name Annotation Expr Expr -- CACHE real name for optimization
--  | Lambda [Pattern] Expr
--  | Call Expr [Expr]
--  | If [(Expr, Expr)] Expr
--  | Let Def Expr
--  | LetRec [Def] Expr
--  | LetDestruct Pattern Expr Expr
--  | Case Expr [CaseBranch]
--  | Accessor Name
--  | Access Expr (A.Located Name)
--  | Update Name Expr (Map.Map Name FieldUpdate)
--  | Record (Map.Map Name Expr)
--  | Unit
--  | Tuple Expr Expr (Maybe Expr)
--  | Shader Shader.Source Shader.Types
--
--
--data CaseBranch =
--  CaseBranch Pattern Expr
--
--
--data FieldUpdate =
--  FieldUpdate A.Region Expr
--
--
--
---- DEFS
--
--
--data Def
--  = Def (A.Located Name) [Pattern] Expr
--  | TypedDef (A.Located Name) FreeVars [(Pattern, Type)] Expr Type
--
--
--
---- DECLARATIONS
--
--
--data Decls
--  = Declare Def Decls
--  | DeclareRec Def [Def] Decls
--  | SaveTheEnvironment
--
--
--
---- PATTERNS
--
--
--type Pattern =
--  A.Located Pattern_
--
--
--data Pattern_
--  = PAnything
--  | PVar Name
--  | PRecord [Name]
--  | PAlias Pattern Name
--  | PUnit
--  | PTuple Pattern Pattern (Maybe Pattern)
--  | PList [Pattern]
--  | PCons Pattern Pattern
--  | PBool Union Bool
--  | PChr ES.String
--  | PStr ES.String
--  | PInt Int
--  | PCtor
--      { _p_home :: ModuleName.Canonical
--      , _p_type :: Name
--      , _p_union :: Union
--      , _p_name :: Name
--      , _p_index :: Index.ZeroBased
--      , _p_args :: [PatternCtorArg]
--      }
--      -- CACHE _p_home, _p_type, and _p_vars for type inference
--      -- CACHE _p_index to replace _p_name in PROD code gen
--      -- CACHE _p_opts to allocate less in PROD code gen
--      -- CACHE _p_alts and _p_numAlts for exhaustiveness checker
--
--
--data PatternCtorArg =
--  PatternCtorArg
--    { _index :: Index.ZeroBased -- CACHE for destructors/errors
--    , _type :: Type             -- CACHE for type inference
--    , _arg :: Pattern
--    }
--
--

-- TYPES


data Annotation = Forall FreeVars Type
  deriving (Eq, Show)


type FreeVars = Map.Map Name ()


data Type
  = TLambda Type Type
  | TVar Name
  | TType ModuleName.Canonical Name [Type]
  | TRecord (Map.Map Name FieldType) (Maybe Name)
  | TUnit
  | TTuple Type Type (Maybe Type)
  | TAlias ModuleName.Canonical Name [(Name, Type)] AliasType
  deriving (Eq, Show)


data AliasType
  = Holey Type
  | Filled Type
  deriving (Eq, Show)


data FieldType = FieldType {-# UNPACK #-} !Word16 Type
  deriving (Eq, Show)


-- NOTE: The Word16 marks the source order, but it may not be available
-- for every canonical type. For example, if the canonical type is inferred
-- the orders will all be zeros.
--
fieldsToList :: Map.Map Name FieldType -> [(Name, Type)]
fieldsToList fields =
  let
    getIndex (_, FieldType index _) =
      index

    dropIndex (name, FieldType _ tipe) =
      (name, tipe)
  in
  map dropIndex (List.sortOn getIndex (Map.toList fields))


--
---- MODULES
--
--
--data Module =
--  Module
--    { _name    :: ModuleName.Canonical
--    , _exports :: Exports
--    , _docs    :: Src.Docs
--    , _decls   :: Decls
--    , _unions  :: Map.Map Name Union
--    , _aliases :: Map.Map Name Alias
--    , _binops  :: Map.Map Name Binop
--    , _effects :: Effects
--    }
--
--
--data Alias = Alias [Name] Type
--  deriving (Eq)
--
--
--data Binop = Binop_ Binop.Associativity Binop.Precedence Name
--  deriving (Eq)
--
--
--data Union =
--  Union
--    { _u_vars :: [Name]
--    , _u_alts :: [Ctor]
--    , _u_numAlts :: Int -- CACHE numAlts for exhaustiveness checking
--    , _u_opts :: CtorOpts -- CACHE which optimizations are available
--    }
--  deriving (Eq)
--
--
--data CtorOpts
--  = Normal
--  | Enum
--  | Unbox
--  deriving (Eq, Ord)
--
--
--data Ctor = Ctor Name Index.ZeroBased Int [Type] -- CACHE length args
--  deriving (Eq)
--
--
--
---- EXPORTS
--
--
--data Exports
--  = ExportEverything A.Region
--  | Export (Map.Map Name (A.Located Export))
--
--
--data Export
--  = ExportValue
--  | ExportBinop
--  | ExportAlias
--  | ExportUnionOpen
--  | ExportUnionClosed
--  | ExportPort
--
--
--
---- EFFECTS
--
--
--data Effects
--  = NoEffects
--  | Ports (Map.Map Name Port)
--  | Manager A.Region A.Region A.Region Manager
--
--
--data Port
--  = Incoming { _freeVars :: FreeVars, _payload :: Type, _func :: Type }
--  | Outgoing { _freeVars :: FreeVars, _payload :: Type, _func :: Type }
--
--
--data Manager
--  = Cmd Name
--  | Sub Name
--  | Fx Name Name
--
--
--
---- BINARY
--
--
--instance Binary Alias where
--  get = liftM2 Alias get get
--  put (Alias a b) = put a >> put b
--
--
--instance Binary Union where
--  put (Union a b c d) = put a >> put b >> put c >> put d
--  get = liftM4 Union get get get get
--
--
--instance Binary Ctor where
--  get = liftM4 Ctor get get get get
--  put (Ctor a b c d) = put a >> put b >> put c >> put d
--
--
--instance Binary CtorOpts where
--  put opts =
--    case opts of
--      Normal -> putWord8 0
--      Enum   -> putWord8 1
--      Unbox  -> putWord8 2
--
--  get =
--    do  n <- getWord8
--        case n of
--          0 -> return Normal
--          1 -> return Enum
--          2 -> return Unbox
--          _ -> fail "binary encoding of CtorOpts was corrupted"
--
--
--instance Binary Annotation where
--  get = liftM2 Forall get get
--  put (Forall a b) = put a >> put b
--
--
--instance Binary Type where
--  put tipe =
--    case tipe of
--      TLambda a b        -> putWord8 0 >> put a >> put b
--      TVar a             -> putWord8 1 >> put a
--      TRecord a b        -> putWord8 2 >> put a >> put b
--      TUnit              -> putWord8 3
--      TTuple a b c       -> putWord8 4 >> put a >> put b >> put c
--      TAlias a b c d     -> putWord8 5 >> put a >> put b >> put c >> put d
--      TType home name ts ->
--        let potentialWord = length ts + 7 in
--        if potentialWord <= fromIntegral (maxBound :: Word8) then
--          do  putWord8 (fromIntegral potentialWord)
--              put home
--              put name
--              mapM_ put ts
--        else
--          putWord8 6 >> put home >> put name >> put ts
--
--  get =
--    do  word <- getWord8
--        case word of
--          0 -> liftM2 TLambda get get
--          1 -> liftM  TVar get
--          2 -> liftM2 TRecord get get
--          3 -> return TUnit
--          4 -> liftM3 TTuple get get get
--          5 -> liftM4 TAlias get get get get
--          6 -> liftM3 TType get get get
--          n -> liftM3 TType get get (replicateM (fromIntegral (n - 7)) get)
--
--
--instance Binary AliasType where
--  put aliasType =
--    case aliasType of
--      Holey tipe  -> putWord8 0 >> put tipe
--      Filled tipe -> putWord8 1 >> put tipe
--
--  get =
--    do  n <- getWord8
--        case n of
--          0 -> liftM Holey get
--          1 -> liftM Filled get
--          _ -> fail "binary encoding of AliasType was corrupted"
--
--
--instance Binary FieldType where
--  get = liftM2 FieldType get get
--  put (FieldType a b) = put a >> put b
--
--
---- [drathier]: binary instances for full canonical module
--
--instance Binary Module where
--  get = liftM8 Module get get get get get get get get
--  put (Module v1 v2 v3 v4 v5 v6 v7 v8) = put v1 >> put v2 >> put v3 >> put v4 >> put v5 >> put v6 >> put v7 >> put v8
--
--
--liftM6  :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m r
--liftM6 f m1 m2 m3 m4 m5 m6 = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; x6 <- m6; return (f x1 x2 x3 x4 x5 x6) }
--
--liftM8  :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m a7 -> m a8 -> m r
--liftM8 f m1 m2 m3 m4 m5 m6 m7 m8 = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; x6 <- m6; x7 <- m7; x8 <- m8; return (f x1 x2 x3 x4 x5 x6 x7 x8) }
--
--
--instance Binary Exports where
--  put exports =
--    case exports of
--      ExportEverything reg  -> putWord8 0 >> put reg
--      Export exports -> putWord8 1 >> put exports
--
--  get =
--    do  n <- getWord8
--        case n of
--          0 -> liftM ExportEverything get
--          1 -> liftM Export get
--          _ -> fail "binary encoding of Exports was corrupted"
--
--
---- [drathier]
--
--instance Binary Src.Docs where
--  put docs =
--    case docs of
--      Src.NoDocs reg  -> putWord8 0 >> put reg
--      Src.YesDocs comment nameCommentPairs -> putWord8 1 >> put comment >> put nameCommentPairs
--
--  get =
--    do  n <- getWord8
--        case n of
--          0 -> liftM Src.NoDocs get
--          1 -> liftM2 Src.YesDocs get get
--          _ -> fail "binary encoding of Src.Docs was corrupted"
--
--instance Binary Src.Comment where
--  get = do
--    bsm <- (get :: Get BS.ByteString)
--    let bs = "{-|" <> bsm <> "-}"
--    case P.fromByteString (
--      Space.docComment
--        (\r p -> (r,p,"Binary Src.Comment parser failed1"))
--        (\_ r p -> (r,p,"Binary Src.Comment parser failed2"))
--      )
--      (\r p -> (r, p,  "Binary Src.Comment parser failed3")) bs of
--      Left err -> error (show (err, "got left from Binary Src.Comment get", bs))
--      Right comment ->
--        return $ comment
--  put (Src.Comment a) = do
--    put (BSL.toStrict (B.toLazyByteString (Utf8.toBuilder (Utf8.fromSnippet a :: Name))) :: BS.ByteString)
--
--
--instance Binary Decls where
--  put docs =
--    case docs of
--      Declare def decls  -> putWord8 0 >> put def >> put decls
--      DeclareRec def rdefs decls -> putWord8 1 >> put def >> put rdefs >> put decls
--      SaveTheEnvironment -> putWord8 2
--
--  get =
--    do  n <- getWord8
--        case n of
--          0 -> liftM2 Declare get get
--          1 -> liftM3 DeclareRec get get get
--          2 -> pure SaveTheEnvironment
--          _ -> fail "binary encoding of Decls was corrupted"
--
--instance Binary Def where
--  put docs =
--    case docs of
--      Def name pats expr -> putWord8 0 >> put name >> put pats >> put expr
--      TypedDef name freeVars tpats expr tipe -> putWord8 1 >> put name >> put freeVars >> put tpats >> put expr >> put tipe
--
--  get =
--    do  n <- getWord8
--        case n of
--          0 -> liftM3 Def get get get
--          1 -> liftM5 TypedDef get get get get get
--          _ -> fail "binary encoding of Def was corrupted"
--
--instance Binary Pattern_ where
--  put pattern =
--    case pattern of
--      PAnything -> putWord8 0
--      PVar name -> putWord8 1 >> put name
--      PRecord name -> putWord8 2 >> put name
--      PAlias name pat -> putWord8 3 >> put name >> put pat
--      PUnit -> putWord8 4
--      PTuple p1 p2 mp3 -> putWord8 5 >> put p1 >> put p2 >> put mp3
--      PList pats -> putWord8 6 >> put pats
--      PCons p1 p2 -> putWord8 7 >> put p1 >> put p2
--      PBool u b -> putWord8 8 >> put u >> put b
--      PChr s -> putWord8 9 >> put s
--      PStr s -> putWord8 10 >> put s
--      PInt i -> putWord8 11 >> put i
--      PCtor home type_ union name index args -> putWord8 12 >> put home >> put type_ >> put union >> put name >> put index >> put args
--
--  get =
--    do  n <- getWord8
--        case n of
--          0 -> pure PAnything
--          1 -> liftM PVar get
--          2 -> liftM PRecord get
--          3 -> liftM2 PAlias get get
--          4 -> pure PUnit
--          5 -> liftM3 PTuple get get get
--          6 -> liftM PList get
--          7 -> liftM2 PCons get get
--          8 -> liftM2 PBool get get
--          9 -> liftM PChr get
--          10 -> liftM PStr get
--          11 -> liftM PInt get
--          12 -> liftM6 PCtor get get get get get get
--          _ -> fail "binary encoding of Pattern_ was corrupted"
--
--instance Binary Expr_ where
--  put expr =
--    case expr of
--      VarLocal name -> putWord8 0 >> put name
--      VarTopLevel home name -> putWord8 1 >> put home >> put name
--      VarKernel modu name -> putWord8 2 >> put modu >> put name
--      VarForeign modu name annot -> putWord8 3 >> put modu >> put name >> put annot
--      VarCtor ctorOpts modu name idx annot -> putWord8 4 >> put ctorOpts >> put modu >> put name >> put idx >> put annot
--      VarDebug modu name annot -> putWord8 5 >> put modu >> put name >> put annot
--      VarOperator op modu name annot -> putWord8 6 >> put op >> put modu >> put name >> put annot
--      Chr c -> putWord8 7 >> put c
--      Str s -> putWord8 8 >> put s
--      Int int -> putWord8 9 >> put int
--      Float float -> putWord8 10 >> put float
--      List exprs -> putWord8 11 >> put exprs
--      Negate expr -> putWord8 12 >> put expr
--      Binop op modu name annot left right -> putWord8 13 >> put op >> put modu >> put name >> put annot >> put left >> put right
--      Lambda pats expr -> putWord8 14 >> put pats >> put expr
--      Call expr args -> putWord8 15 >> put expr >> put args
--      If cases elseExpr -> putWord8 16 >> put cases >> put elseExpr
--      Let def expr -> putWord8 17 >> put def >> put expr
--      LetRec defs expr -> putWord8 18 >> put defs >> put expr
--      LetDestruct pat expr body -> putWord8 19 >> put pat >> put expr >> put body
--      Case expr cases -> putWord8 20 >> put expr >> put cases
--      Accessor name -> putWord8 21 >> put name
--      Access expr name -> putWord8 22 >> put expr >> put name
--      Update name expr fields -> putWord8 23 >> put name >> put expr >> put fields
--      Record fields -> putWord8 24 >> put fields
--      Unit -> putWord8 25
--      Tuple e1 e2 me3 -> putWord8 26 >> put e1 >> put e2 >> put me3
--      Shader src tipes -> putWord8 27 >> put src >> put tipes
--
--  get =
--    do  n <- getWord8
--        case n of
--          0 -> liftM VarLocal get
--          1 -> liftM2 VarTopLevel get get
--          2 -> liftM2 VarKernel get get
--          3 -> liftM3 VarForeign get get get
--          4 -> liftM5 VarCtor get get get get get
--          5 -> liftM3 VarDebug get get get
--          6 -> liftM4 VarOperator get get get get
--          7 -> liftM Chr get
--          8 -> liftM Str get
--          9 -> liftM Int get
--          10 -> liftM Float get
--          11 -> liftM List get
--          12 -> liftM Negate get
--          13 -> liftM6 Binop get get get get get get
--          14 -> liftM2 Lambda get get
--          15 -> liftM2 Call get get
--          16 -> liftM2 If get get
--          17 -> liftM2 Let get get
--          18 -> liftM2 LetRec get get
--          19 -> liftM3 LetDestruct get get get
--          20 -> liftM2 Case get get
--          21 -> liftM Accessor get
--          22 -> liftM2 Access get get
--          23 -> liftM3 Update get get get
--          24 -> liftM Record get
--          25 -> pure Unit
--          26 -> liftM3 Tuple get get get
--          27 -> liftM2 Shader get get
--          _ -> fail "binary encoding of Expr_ was corrupted"
--
--instance Binary PatternCtorArg where
--  get = liftM3 PatternCtorArg get get get
--  put (PatternCtorArg a b c) = put a >> put b >> put c
--
--instance Binary CaseBranch where
--  get = liftM2 CaseBranch get get
--  put (CaseBranch a b) = put a >> put b
--
--instance Binary FieldUpdate where
--  get = liftM2 FieldUpdate get get
--  put (FieldUpdate a b) = put a >> put b
--
--instance Binary Binop where
--  get = liftM3 Binop_ get get get
--  put (Binop_ a b c) = put a >> put b >> put c
--
--instance Binary Effects where
--  put effects =
--    case effects of
--      NoEffects -> putWord8 0
--      Ports ports -> putWord8 1 >> put ports
--      Manager a b c manager ->
--        putWord8 2 >> put a >> put b >> put c >> put manager
--
--  get =
--    do  n <- getWord8
--        case n of
--          0 -> return NoEffects
--          1 -> liftM Ports get
--          2 -> liftM4 Manager get get get get
--          _ -> fail "binary encoding of Effects was corrupted"
--
--instance Binary Port where
--  put port =
--    case port of
--      Incoming freeVars payload func -> putWord8 0 >> put freeVars >> put payload >> put func
--      Outgoing freeVars payload func -> putWord8 1 >> put freeVars >> put payload >> put func
--  get =
--    do  n <- getWord8
--        case n of
--          0 -> liftM3 Incoming get get get
--          1 -> liftM3 Outgoing get get get
--          _ -> fail "binary encoding of Port was corrupted"
--
--instance Binary Manager where
--  put manager =
--    case manager of
--      Cmd name -> putWord8 0 >> put name
--      Sub name -> putWord8 1 >> put name
--      Fx n1 n2 -> putWord8 2 >> put n1 >> put n2
--  get =
--    do  n <- getWord8
--        case n of
--          0 -> liftM Cmd get
--          1 -> liftM Sub get
--          2 -> liftM2 Fx get get
--          _ -> fail "binary encoding of Manager was corrupted"
--
--instance Binary Export where
--  put docs =
--    case docs of
--      ExportValue -> putWord8 0
--      ExportBinop -> putWord8 1
--      ExportAlias -> putWord8 2
--      ExportUnionOpen -> putWord8 3
--      ExportUnionClosed -> putWord8 4
--      ExportPort -> putWord8 5
--
--  get =
--    do  n <- getWord8
--        case n of
--          0 -> pure ExportValue
--          1 -> pure ExportBinop
--          2 -> pure ExportAlias
--          3 -> pure ExportUnionOpen
--          4 -> pure ExportUnionClosed
--          5 -> pure ExportPort
--          _ -> fail "binary encoding of Export was corrupted"
