{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- |
-- This module generates code in the simplified Erlang intermediate representation from Purescript code
module Language.PureScript.Erl.CodeGen
  ( module AST,
    moduleToErl,
    buildCodegenEnvironment,
    CodegenEnvironment,
  )
where

import Control.Applicative ((<|>))
import Control.Arrow (first, second)
import Control.Monad (foldM, replicateM, unless)
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Supply.Class (MonadSupply (fresh), bumpToNextRoundNumber)
import Control.Monad.Writer (MonadWriter (..), WriterT (runWriterT))
import Data.Monoid (Any(..))
import Data.Either (fromRight)
import Data.Foldable (find, traverse_, foldl')
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Traversable (forM)
import Debug.Trace (trace, traceM)
-- import qualified Language.PureScript as P
import Language.PureScript.AST (SourceSpan, nullSourceSpan)
import qualified Language.PureScript.Constants.Libs as C
import qualified Language.PureScript.Constants.Prim as C
import Language.PureScript.CoreFn
  ( Ann,
    Bind (..),
    Binder (..),
    CaseAlternative (CaseAlternative, caseAlternativeBinders),
    Expr (..),
    Literal (..),
    Meta (IsConstructor, IsNewtype, IsTypeClassConstructor, IsSyntheticApp),
    Module (Module),
    everywhereOnValues,
    extractAnn,
    ssAnn,
  )
import Language.PureScript.Environment as E
  ( Environment (names, typeSynonyms, types),
    tyFunction,
  )
import Language.PureScript.Erl.CodeGen.AST as AST
-- import Language.PureScript.Erl.CodeGen.CheckedWrapper (typecheckWrapper)
import Language.PureScript.Erl.CodeGen.Common
  ( ModuleType (ForeignModule, PureScriptModule),
    atomModuleName,
    freshNameErl,
    freshNameErl',
    identToVar,
    toAtomName, identToAtomName, runIdent', runAtom
  )
import Language.PureScript.Erl.CodeGen.Constants.PureScriptModules
  ( dataFunctionUncurried,
    effectUncurried,
  )
import Language.PureScript.Erl.Errors (MultipleErrors, addHint, errorMessage, rethrow, rethrowWithPosition)
import Language.PureScript.Erl.Errors.Types
  ( SimpleErrorMessage
      ( InvalidFFIArity,
        MissingFFIImplementations,
        UnusedFFIImplementations
      ),
  )
import Language.PureScript.Erl.Synonyms (replaceAllTypeSynonyms')
import Language.PureScript.Errors (ErrorMessageHint (..))
import Language.PureScript.Names
  ( Ident (Ident, UnusedIdent, InternalIdent),
    ModuleName (..),
    ProperName (ProperName),
    Qualified (..),
    InternalIdentData (RuntimeLazyFactory, Lazy)
  )
import Language.PureScript.Options (Options)
import Language.PureScript.Traversals (sndM)
import Language.PureScript.Types
  ( SourceType,
    Type (..),
  )
import Prelude.Compat
import Language.PureScript.Erl.CodeGen.Types (ETypeEnv, uncurriedFnTypes, replaceVars, translateType, uncurryType)
import Language.PureScript.CoreFn.Laziness (applyLazinessTransform)
-- import Language.PureScript (internalError)
-- ### purserl

-- import qualified Language.PureScript as P
import qualified Control.Monad.Supply as P
import qualified Data.Version (Version)
import qualified Language.PureScript.AST as P
import qualified Language.PureScript.Comments as P
import qualified Language.PureScript.Crash as P
import qualified Language.PureScript.Environment as P
import qualified Language.PureScript.Errors as P hiding (indent)
import qualified Language.PureScript.Externs as P
import qualified Language.PureScript.Linter as P
import qualified Language.PureScript.ModuleDependencies as P
import qualified Language.PureScript.Names as P
import qualified Language.PureScript.Options as P
import qualified Language.PureScript.Pretty as P
import qualified Language.PureScript.Renamer as P
import qualified Language.PureScript.Roles as P
import qualified Language.PureScript.Sugar as P
import qualified Language.PureScript.TypeChecker as P
import qualified Language.PureScript.Types as P

-- import Language.PureScript (internalError)
import Language.PureScript.Crash (internalError)
--
import Language.PureScript.CoreFn.Expr qualified as E
import Control.Monad.State (StateT(..), runStateT, mapStateT, lift, get, put)

import Language.PureScript.PSString qualified as PS
import Language.PureScript.AST.SourcePos qualified as SS

identToTypeclassCtor :: Ident -> Atom
identToTypeclassCtor a = Atom Nothing (runIdent' a)

qualifiedToErl' :: ModuleName -> ModuleType -> Ident -> Atom
qualifiedToErl' mn' moduleType ident = Atom (Just $ atomModuleName mn' moduleType) (runIdent' ident)

-- Top level definitions are everywhere fully qualified, variables are not.
qualifiedToErl :: ModuleName -> Qualified Ident -> Atom
qualifiedToErl mn (Qualified (P.ByModuleName mn') ident)
  -- Local reference to local non-exported function
  | mn == mn' -- TODO making all local calls non qualified - for memoization onload - revert
  -- && ident `Set.notMember` declaredExportsSet  =
    =
    Atom Nothing (runIdent' ident)
  -- Reference other modules or exported things via module name
  | otherwise =
    qualifiedToErl' mn' PureScriptModule ident
qualifiedToErl _ (Qualified (P.BySourcePos _) ident) = Atom Nothing (runIdent' ident)




uncurriedFnArity :: ModuleName -> T.Text -> SourceType -> Maybe Int
uncurriedFnArity moduleName fnName ty = fst <$> uncurriedFnTypes moduleName fnName ty

concatRes :: [([a], [b], ETypeEnv)] -> ([a], [b], ETypeEnv)
concatRes x = (concatMap (\(a, _, _) -> a) x, concatMap (\(_, b, _) -> b) x, M.unions $ (\(_, _, c) -> c) <$> x)

isFullySaturatedForeignCall :: ModuleName -> M.Map (Qualified Ident) Int -> Expr Ann -> [a] -> Bool
isFullySaturatedForeignCall mn actualForeignArities var args = case var of
  Var _ qi
    | P.isQualifiedWith mn qi,
      Just arity <- M.lookup qi actualForeignArities,
      length args == arity ->
      True
  _ -> False


data FnArity = EffFnXArity Int | FnXArity Int | Arity (Int, Int)
  deriving (Eq, Show)



data CodegenEnvironment = CodegenEnvironment E.Environment (M.Map (Qualified Ident) FnArity)

buildCodegenEnvironment :: E.Environment -> CodegenEnvironment
buildCodegenEnvironment env = CodegenEnvironment env explicitArities
  where
    tyArity :: SourceType -> FnArity
    tyArity t = Arity $ go 0 t'
      where
        t' = fromRight t $ replaceAllTypeSynonyms' (E.typeSynonyms env) (E.types env) t

        go n = \case
          ConstrainedType _ _ ty -> go (n + 1) ty
          ForAll _ _ _ _ ty _ -> go n ty
          other -> (n, go' other)
        go' = \case
          TypeApp _ (TypeApp _ fn _) ty | fn == E.tyFunction -> 1 + go' ty
          ForAll _ _ _ _ ty _ -> go' ty
          _ -> 0

    explicitArities :: M.Map (Qualified Ident) FnArity
    explicitArities = tyArity <$> types

    types :: M.Map (Qualified Ident) SourceType
    types = M.map (\(t, _, _) -> t) $ E.names env

data Arities = Arities
  { _effective :: M.Map (Qualified Ident) FnArity
  , _used :: M.Map (Qualified Ident) (Set Int)
  , _actualForeign :: M.Map (Qualified Ident) Int
  }

findArities :: ModuleName -> [Bind Ann] -> CodegenEnvironment -> [(T.Text, Int)] -> Arities
findArities mn decls (CodegenEnvironment _ explicitArities) foreignExports = Arities arities usedArities actualForeignArities
  where
  actualForeignArities :: M.Map (Qualified Ident) Int
  actualForeignArities = M.fromList $ map (\(x, n) -> (Qualified (P.ByModuleName mn) (Ident x), n)) foreignExports

  arities :: M.Map (Qualified Ident) FnArity
  arities =
    -- max arities is max of actual impl and most saturated application
    let inferredArities = foldr findUsages (Set.singleton <$> actualForeignArities) decls
        inferredMaxArities = M.mapMaybe (fmap (\n -> Arity (0, n)) . Set.lookupMax) inferredArities
      in explicitArities `M.union` inferredMaxArities

  usedArities :: M.Map (Qualified Ident) (Set Int)
  usedArities = foldr findUsages M.empty decls

  findUsages :: Bind Ann -> M.Map (Qualified Ident) (Set Int) -> M.Map (Qualified Ident) (Set Int)
  findUsages (NonRec _ _ val) apps = findUsages' val apps
  findUsages (Rec vals) apps = foldr (findUsages' . snd) apps vals

  findUsages' :: Expr Ann -> M.Map (Qualified Ident) (Set Int) -> M.Map (Qualified Ident) (Set Int)
  findUsages' expr apps = case expr of
    e@App {} ->
      let (f, args) = unApp e []
          apps' = foldr findUsages' apps args
        in case f of
            Var (_, _, Just IsNewtype) _ -> apps'
            -- This is an app but we inline these
            Var (_, _, Just (IsConstructor _ fields)) (Qualified _ _)
              | length args == length fields ->
                apps'
            Var (_, _, Just IsTypeClassConstructor) _ ->
              apps'
            -- Don't count fully saturated foreign import call, it will be called directly
            Var _ _
              | isFullySaturatedForeignCall mn actualForeignArities f args ->
                apps'
            Var _ (Qualified q ident)
              | thisModule q ->
                M.insertWith Set.union (Qualified (P.ByModuleName mn) ident) (Set.singleton $ length args) apps'
            _ -> findUsages' f apps'
    v@Var {} ->
      case v of
        -- Must actually not assume this is 0 as it may be a ref fn/1 or fnx/efffnx/n
        -- Should record separately - or assume that 0 may mean non-0 for this reason?
        Var _ (Qualified q ident)
          | thisModule q ->
            M.insertWith Set.union (Qualified (P.ByModuleName mn) ident) (Set.singleton 0) apps
        _ -> apps
    Accessor _ _ e -> findUsages' e apps
    ObjectUpdate _ e _ es -> findUsages' e $ foldr (findUsages' . snd) apps es
    Abs _ _ e -> findUsages' e apps
    Case _ e es -> foldr findUsages' (foldr findUsagesCase apps es) e
    Let _ b e' ->
      findUsages' e' $ foldr findUsages'' apps b
    Literal _ litExpr -> case litExpr of
      NumericLiteral _ -> apps
      StringLiteral _ -> apps
      CharLiteral _ -> apps
      BooleanLiteral _ -> apps
      ArrayLiteral exprs -> foldr findUsages' apps exprs
      ObjectLiteral fields -> foldr (findUsages' . snd) apps fields
    Constructor {} -> apps
    where
      unApp :: Expr Ann -> [Expr Ann] -> (Expr Ann, [Expr Ann])
      unApp (App _ val arg) args = unApp val (arg : args)
      unApp other args = (other, args)

      thisModule (P.ByModuleName mn') = mn' == mn
      thisModule (P.BySourcePos _) = True

  findUsages'' (NonRec _ _ e) apps = findUsages' e apps
  findUsages'' (Rec binds) apps = foldr (findUsages' . snd) apps binds

  findUsagesCase (CaseAlternative _ (Right e)) apps = findUsages' e apps
  findUsagesCase (CaseAlternative _ (Left ges)) apps = foldr findUsages' apps $ concatMap (\(a, b) -> [a, b]) ges


-- |
-- Generate code in the simplified Erlang intermediate representation for all declarations in a
-- module.
moduleToErl ::
  forall m.
  (Monad m, MonadReader Options m, MonadSupply m, MonadError MultipleErrors m, MonadWriter MultipleErrors m) =>
  CodegenEnvironment ->
  Module Ann ->
  [(T.Text, Int)] ->
  m ([(Atom, Int)], [Erl], [Erl], [Erl], [(Atom, Int)], [Erl]) -- , Map Atom Int)
moduleToErl codegenEnv m@(Module _ _ mn _ _ _ _ _ _) foreignExports =
  rethrow (addHint (ErrorInModule mn)) $ do
    (res, (warnings, Any needRuntimeLazy)) <- runWriterT $ moduleToErl' codegenEnv m foreignExports
    tell warnings

    pure $ if needRuntimeLazy then
      let (exports, namedSpecs, foreignSpecs, decls, safeExports, safeDecls) = res

      in (exports, namedSpecs, foreignSpecs, runtimeLazy : runtimeLazyCurried : decls, safeExports, safeDecls)
    else
      res

  where

  -- Lazy initialisation runtime - we almost don't need this at all, except in the case of an insufficiently lazy recursive reference,
  -- we would naturally recurse forever instead of throwing as specced. Given most instances don't require this, the overhead may be worth
  -- finding an explicit error instead of hang
  runtimeLazy :: Erl
  runtimeLazy = EFunctionDef Nothing Nothing (Atom Nothing $ identToAtomName $ InternalIdent RuntimeLazyFactory) ["CtxRef", "Name", "ModuleName", "Init"] runtimeLazyBody

  -- TODO I don't want to need this
  runtimeLazyCurried :: Erl
  runtimeLazyCurried = EFunctionDef Nothing Nothing (Atom Nothing $ identToAtomName $ InternalIdent RuntimeLazyFactory) [ "CtxRef" ] $
    EFunFull Nothing [(EFunBinder [EVar "Name", EVar "ModuleName", EVar "Init"], runtimeLazyBody)]

  runtimeLazyBody :: Erl
  runtimeLazyBody =
    EBlock
      [
        -- TODO This means this is never cached at the top level. In fact it might simply not work
        EVarBind "StateKey" (ETupleLiteral [ EVar "Name", EVar "ModuleName", EAtomLiteral (Atom Nothing "lazy_state_@purerl"), EVar "CtxRef" ])
      , EVarBind "ValueKey" (ETupleLiteral [ EVar "Name", EVar "ModuleName", EAtomLiteral (Atom Nothing "lazy_value_@purerl"), EVar "CtxRef" ])
      , EFun1 Nothing "LineNo"
         ( EBlock
          [ ECaseOf (qualFunCall "erlang" "get" [EVar "StateKey"])
            [ ( EBinder $ litAtom "initialized", qualFunCall "erlang" "get" [EVar "ValueKey"] )
            , ( EBinder $ litAtom "initializing", qualFunCall "erlang" "throw" [ETupleLiteral [ litAtom "not_finished_initializing", EVar "Name", EVar "ModuleName", EVar "LineNo" ] ] )
            , ( EBinder $ litAtom "undefined", EBlock
                [ qualFunCall "erlang" "put" [ EVar "StateKey", litAtom "initializing" ]
                , EVarBind "Value" (EApp RegularApp (EVar "Init") [ litAtom "unit" ])
                , qualFunCall "erlang" "put" [ EVar "ValueKey", EVar "Value" ]
                , qualFunCall "erlang" "put" [ EVar "StateKey", litAtom "initialized" ]
                , EVar "Value"
                ]
              )
            ]
          ]
         )
      ]

moduleToErl' ::
  forall m.
  (Monad m, MonadReader Options m, MonadSupply m, MonadError MultipleErrors m, MonadWriter (MultipleErrors, Any) m) =>
  CodegenEnvironment ->
  Module Ann ->
  [(T.Text, Int)] ->
  m ([(Atom, Int)], [Erl], [Erl], [Erl], [(Atom, Int)], [Erl]) -- , Map Atom Int)
moduleToErl' cgEnv@(CodegenEnvironment env explicitArities) (Module _ _ mn _ _ declaredExports _ foreigns origDecls) foreignExports =
  do
    res <- traverse (\b ->
      do
        bumpToNextRoundNumber
        topBindToErl b
      ) decls
    reexports <- traverse reExportForeign foreigns
    let exportTypes = mapMaybe (\(_, _, t, _) -> t) reexports
        foreignSpecs = map (\(ident, ty) -> ESpec (qualifiedToErl' mn ForeignModule ident) (replaceVars ty)) exportTypes

        (exports, erlDecls, typeEnv) = concatRes $ res <> map (\(a, b, _, d) -> (a, b, d)) reexports
        namedSpecs = map (\(name, (args, ty)) -> EType (Atom Nothing name) args ty) $ M.toList typeEnv

    traverse_ checkExport foreigns
    let usedFfi = Set.fromList $ map runIdent' foreigns
        definedFfi = Set.fromList (map fst foreignExports)
        unusedFfi = definedFfi Set.\\ usedFfi
    unless (Set.null unusedFfi) $
      tell (errorMessage $ UnusedFFIImplementations mn (Ident <$> Set.toAscList unusedFfi), mempty)

    let attributes = findAttributes decls

    -- safeDecls <- concat <$> traverse (typecheckWrapper mn) erlDecls
    safeDecls <- pure mempty


    let fnl (EFunctionDef _ _ fnName args _) = Just (fnName, length args)
        fnl _ = Nothing
        safeExports = mapMaybe fnl safeDecls

        -- memoizable =
        --   M.mapKeys (qualifiedToErl mn) $
        --     M.mapMaybe
        --       ( \case
        --           Arity (n, _) | n > 0 -> Just n
        --           _ -> Nothing
        --       )
        --       arities
        --       -- Var _ qi@(Qualified _ _)
    return (exports, namedSpecs, foreignSpecs, attributes ++ erlDecls, safeExports, safeDecls)
  where
    declaredExportsSet = Set.fromList declaredExports

    Arities arities usedArities actualForeignArities = findArities mn decls cgEnv foreignExports

    decls :: [Bind Ann]
    decls = go <$> origDecls
      where
        go (NonRec ann ident val) = NonRec ann ident (removeDollars val)
        go (Rec vals) = Rec $ map (second removeDollars) vals

        removeDollars = fe
          where
            (_, fe, _) = everywhereOnValues id go id

            go (App ann (App _ (Var _ apply) f) a)
              | C.I_functionApply <- apply = App ann f a
            go (App ann (App _ (Var _ apply) a) f)
              | C.I_functionApplyFlipped <- apply = App ann f a
            go other = other

    types :: M.Map (Qualified Ident) SourceType
    types = M.map (\(t, _, _) -> t) $ E.names env

    findAttributes :: [Bind Ann] -> [Erl]
    findAttributes expr = map (uncurry EAttribute) $ mapMaybe getAttribute $ concatMap onBind expr
      where
        getAttribute (TypeApp _ (TypeApp _ (TypeConstructor _ (Qualified (P.ByModuleName _) (ProperName "Attribute"))) (TypeLevelString _ a)) (TypeLevelString _ b)) =
          Just (a, b)
        getAttribute _ = Nothing

        getType ident = M.lookup (Qualified (P.ByModuleName mn) ident) types

        onRecBind ((_, ident), _) = getType ident
        onBind (NonRec _ ident _) = catMaybes [getType ident]
        onBind (Rec vals) = mapMaybe onRecBind vals



    -- 're-export' foreign imports in the @ps module - also used for internal calls for non-exported foreign imports
    reExportForeign :: Ident -> m ([(Atom, Int)], [Erl], Maybe (Ident, EType), ETypeEnv)
    reExportForeign ident = do
      let arity = exportArity ident
          fullArity = case M.lookup (Qualified (P.ByModuleName mn) ident) arities of
            Just (Arity (0, n)) -> n
            _ -> arity
          wrapTy (ty', tenv) = case arity of
            0 -> Just (TFun [] ty', tenv)
            _ -> (,tenv) <$> uncurryType arity ty'

          ffiTyEnv = wrapTy . translateType env =<< M.lookup (Qualified (P.ByModuleName mn) ident) types

      args <- replicateM fullArity freshNameErl
      let body = EApp RegularApp (EAtomLiteral $ qualifiedToErl' mn ForeignModule ident) (take arity $ map EVar args)
          body' = curriedApp (drop arity $ map EVar args) body
          fun = curriedLambda body' args
      fident <- fmap (Ident . ("f" <>) . T.pack . show) fresh
      let var = Qualified P.ByNullSourcePos fident
          -- wrap e = EBlock [EVarBind (identToVar fident) fun, e]
          wrap e = ELet (EVarBind (identToVar fident) fun) e
      (idents, erl, env) <- generateFunctionOverloads Nothing True Nothing (ssAnn nullSourceSpan) ident (Atom Nothing $ runIdent' ident) (Var (ssAnn nullSourceSpan) var) wrap
      let combinedTEnv = M.union env (maybe M.empty snd ffiTyEnv)
      pure (idents, erl, (ident,) . fst <$> ffiTyEnv, combinedTEnv)



    exportArity :: Ident -> Int
    exportArity ident = fromMaybe 0 $ findExport $ runIdent' ident

    checkExport :: Ident -> m ()
    checkExport ident =
      case (findExport (runIdent' ident), M.lookup (Qualified (P.ByModuleName mn) ident) explicitArities) of
        -- TODO is it meaningful to check against inferred arities (as we are just now) or only explicit ones
        -- This probably depends on the current codegen

        -- If we know the foreign import type (because it was exported) and the actual FFI type, it is an error if
        -- the actual implementation has higher arity than the type does
        -- If the actual implementation has lower arity, it may be just returning a function
        (Just m, Just (Arity (nc, n)))
          | m > nc + n ->
            throwError . errorMessage $ InvalidFFIArity mn (runIdent' ident) m (nc + n)
        -- If we don't know the declared type of the foreign import (because it is not exported), then we cannot say
        -- what the implementation's arity should be, as it may be higher than can be inferred from applications in this
        -- module (because there is no fully saturated application) or lower (because the ffi returns a function)
        (Just _, Nothing) ->
          pure ()
        -- We certainly can tell if an import exists and the implementation isn't found
        -- The opposite situation is handled at the top level of moduleToErl
        (Nothing, _) ->
          throwError . errorMessage $ MissingFFIImplementations mn [ident]
        _ -> pure ()

    findExport :: T.Text -> Maybe Int
    findExport n = snd <$> find ((n ==) . fst) foreignExports

    isTopLevelBinding :: Qualified Ident -> Bool
    isTopLevelBinding (Qualified (P.ByModuleName _) _) = True
    isTopLevelBinding (Qualified (P.BySourcePos _) (InternalIdent (Lazy ident))) = Ident ident `elem` topLevelNames
    isTopLevelBinding (Qualified (P.BySourcePos _) (InternalIdent RuntimeLazyFactory)) = True
    isTopLevelBinding (Qualified (P.BySourcePos _) ident) = ident `elem` topLevelNames

    topLevelNames = concatMap topLevelName decls
      where
        topLevelName :: Bind Ann -> [Ident]
        topLevelName (NonRec _ ident _) = [ident]
        topLevelName (Rec vals) = map (snd . fst) vals

    uncurriedFnArity' :: (Int -> FnArity) -> ModuleName -> T.Text -> Qualified Ident -> Maybe FnArity
    uncurriedFnArity' ctor fnMod fn ident =
      case M.lookup ident types of
        Just t -> ctor <$> uncurriedFnArity fnMod fn t
        _ -> Nothing

    effFnArity = uncurriedFnArity' EffFnXArity effectUncurried "EffectFn"
    fnArity = uncurriedFnArity' FnXArity dataFunctionUncurried "Fn"

    topBindToErl :: (Bind Ann -> m ([(Atom, Int)], [Erl], ETypeEnv))
    topBindToErl = \case
      NonRec ann ident val -> topNonRecToErl False ann ident val
      Rec vals ->
        let (vals', needRuntimeLazy@(Any needLazyRef)) = applyLazinessTransform mn vals
        in
          concatRes <$>
          (writer (vals', (mempty, needRuntimeLazy)) >>=
              traverse (uncurry . uncurry $ topNonRecToErl needLazyRef))


    topNonRecToErl :: Bool -> Ann -> Ident -> Expr Ann -> m ([(Atom, Int)], [Erl], ETypeEnv)
    topNonRecToErl inLazyRecGroup (ss, _, _) ident val = do
      let eann@(_, _, meta') = extractAnn val
          ident' = case meta' of
            Just IsTypeClassConstructor -> identToTypeclassCtor ident
            _ -> Atom Nothing $ runIdent' ident

      val' <- ensureFreshVars_ val
      (maybeVarName, wrapper) <- if inLazyRecGroup then
        do
          lazyVarName <- freshNameErl' "LazyCtxRef"
          pure (Just lazyVarName, \e -> ELet (EVarBind lazyVarName $ litAtom "top_level") e)
        else
          pure (Nothing, id)
      generateFunctionOverloads maybeVarName False (Just ss) eann ident ident' val' wrapper

    generateFunctionOverloads :: Maybe T.Text -> Bool -> Maybe SourceSpan -> Ann -> Ident -> Atom -> Expr Ann -> (Erl -> Erl) -> m ([(Atom, Int)], [Erl], ETypeEnv)
    generateFunctionOverloads lazyVarName isForeign ss eann ident ident' val outerWrapper = do
      -- Always generate the plain curried form, f x y = ... -~~> f() -> fun (X) -> fun (Y) -> ... end end.
      let qident = Qualified (P.ByModuleName mn) ident
          replaceLazyCall = everywhereOnErl go
            where
              go (EApp RegularApp lazyFactory [ ])
               | lazyFactory == EAtomLiteral (Atom Nothing $ identToAtomName $ InternalIdent RuntimeLazyFactory)
               , Just varName <- lazyVarName
               = EApp RegularApp lazyFactory [ EVar varName ]
              go e = e

      erl <- replaceLazyCall <$> valueToErl val

      let translated = translateType env <$> M.lookup qident types
          erlangType = replaceVars . fst <$> translated
          etypeEnv = maybe M.empty snd translated

      let curried = ([(ident', 0)], [EFunctionDef (TFun [] <$> erlangType) ss ident' [] (outerWrapper erl)])

      -- For effective > 0 (either plain curried funs, FnX or EffectFnX) generate an uncurried overload
      -- f x y = ... -~~> f(X,Y) -> ((...)(X))(Y).
      -- Relying on inlining to clean up some junk here
      let mkRunApp modName prefix n = App eann (Var eann (Qualified (P.ByModuleName modName) (Ident $ prefix <> T.pack (show n))))
          applyStep fn a = App eann fn (Var eann (Qualified P.ByNullSourcePos (Ident a)))

          countAbs :: Expr Ann -> Int
          countAbs (Abs _ _ e) = 1 + countAbs e
          countAbs _ = 0

      let curriedWrappingUncurried arity = do
            vars <- replicateM arity freshNameErl
            let app = EApp RegularApp (EAtomLiteral ident') (EVar <$> vars)
                callUncurriedErl = curriedLambda app vars
            pure
              ([(ident', 0)], [EFunctionDef (TFun [] <$> erlangType) ss ident' [] callUncurriedErl])
      let uncurriedWrappingCurried arity = do
            vars <- replicateM arity freshNameErl
            let callCurriedErl = curriedApp (EVar <$> vars) $ EApp RegularApp (EAtomLiteral ident') []
            pure
              ([(ident', arity)], [EFunctionDef (uncurryType arity =<< erlangType) ss ident' vars callCurriedErl])

      let guard :: Monoid a => Bool -> a -> a
          guard t x = if t then x else mempty

      let checkUsed :: (Set Int -> Bool) -> Bool
          checkUsed f =
            ident `Set.member` declaredExportsSet || isForeign
              || maybe False f (M.lookup qident usedArities)
              || isLazy ident
          isLazy (InternalIdent (Lazy _)) = True
          isLazy (InternalIdent RuntimeLazyFactory) = True
          isLazy _ = False

          lazyArity (InternalIdent (Lazy _)) = Just (Arity (0, 1))
          lazyArity (InternalIdent RuntimeLazyFactory) = Just (Arity (0, 3))
          lazyArity _ = Nothing

          -- NOTE[drathier]: this part of the optimization pipeline assumes we know all the function call site arities. When running this as a single binary, we're doing erl code gen as we go instead of as a separate second pass, so we have to play it safe here and assume all fn arities are used.
          usedArity a = True -- checkUsed (Set.member a)
          usedAnyArity = True -- checkUsed (not . Set.null)
          usedExceptArity a = True -- checkUsed (not . Set.null . Set.delete a)

      -- Apply in CoreFn then translate to take advantage of translation of full/partial application
      (res1, res2) <- case effFnArity qident <|> fnArity qident <|> M.lookup qident arities <|> lazyArity ident of
        Just (EffFnXArity arity) -> do
          vars <- replicateM arity freshNameErl
          erl' <- valueToErl $ foldl applyStep (mkRunApp effectUncurried (snd C.P_runEffectFn) arity val) vars
          pure $ curried <> ([(ident', arity)], [EFunctionDef (uncurryType arity =<< erlangType) ss ident' vars (outerWrapper (EApp RegularApp erl' []))])
        Just (FnXArity arity) -> do
          -- Same as above
          vars <- replicateM arity freshNameErl
          erl' <- valueToErl $ foldl applyStep (mkRunApp dataFunctionUncurried (snd C.P_runFn) arity val) vars
          pure $ curried <> ([(ident', arity)], [EFunctionDef (uncurryType arity =<< erlangType) ss ident' vars (outerWrapper erl')])
        Just (Arity (n, m)) | n + m > 0 ->
          do
            let arity = n + m

            -- experimental split between typeclass & regular arguments
            -- TODO this still duplicates code
            vars <- replicateM arity freshNameErl
            split <-
              if n == 0 || m == 0
                then pure ([], [])
                else do
                  erl'' <- valueToErl $ foldl applyStep val (take n vars)
                  pure ([(ident', n)], [EFunctionDef Nothing ss ident' (take n vars) (outerWrapper erl'')])

            if countAbs val == arity && usedArity arity
              then do
                erl' <- valueToErl $ foldl applyStep val vars
                curriedWrap <- curriedWrappingUncurried arity
                let uncurried = ([(ident', arity)], [EFunctionDef (uncurryType arity =<< erlangType) ss ident' vars (outerWrapper erl')])
                pure $ uncurried <> guard (usedExceptArity arity) curriedWrap <> split
              else
                if usedAnyArity
                  then do
                    uncurriedWrap <- uncurriedWrappingCurried arity
                    pure $ curried <> guard (usedArity arity) uncurriedWrap <> split
                  else do
                    pure ([], [])
        _ ->
          if usedAnyArity
            then pure curried
            else pure mempty
      pure $
        if ident `Set.member` declaredExportsSet
          then (res1, res2, etypeEnv)
          else ([], res2, etypeEnv)

    bindToErl :: Bind Ann -> m (Erl -> Erl)
    bindToErl bind =
      case bind of
        NonRec (ss,_,_) ident val -> do
          b <- EVarBind (identToVar ident) <$> valueToErl'' ss (Just ident) val
          pure (\innermost -> ELet b innermost)
          -- For recursive bindings F(X) = E1, G(X) = E2, ... we have a problem as the variables are not
          -- in scope until each expression is defined. To avoid lifting to the top level first generate
          -- funs which take a tuple of such funs F'({F', G'}) -> (X) -> E1 etc.
          -- with occurences of F, G replaced in E1, E2 with F'({F',G'})
          -- and then bind these F = F'({F',G'})
          -- TODO: Only do this if there are multiple mutually recursive bindings! Else a named fun works.
        Rec origVals -> do
          let (vals, needRuntimeLazy@(Any needLazyRef)) = applyLazinessTransform mn origVals
          tell (mempty, needRuntimeLazy)
          lazyVarName <- freshNameErl' "LazyCtxRef"
          let vars = identToVar . snd . fst <$> vals
              varTupInner = ETupleLiteral $ EVar . (<> "@fi") <$> vars
              varTupOuter = ETupleLiteral $ EVar . (<> "@f") <$> vars

              replaceFun fvar = everywhereOnErl go
                where
                  go (EVar f) | f == fvar = EApp RegularApp (EVar $ f <> "@fi") [varTupInner]
                  go (EApp RegularApp lazyFactory [ ])
                   | lazyFactory == EAtomLiteral (Atom Nothing $ identToAtomName $ InternalIdent RuntimeLazyFactory)
                   , needLazyRef
                   = EApp RegularApp lazyFactory [ EVar lazyVarName ]
                  go e = e

          (funs :: [Erl]) <- forM vals $ \((_, ident), val) -> do
            erl <- valueToErl' Nothing val
            let erl' = foldr replaceFun erl vars
            let fun = EFunFull Nothing [(EFunBinder [varTupInner], erl')]
            pure $ EVarBind (identToVar ident <> "@f") fun
          let rebinds = map (\var -> EVarBind var (EApp RegularApp (EVar $ var <> "@f") [varTupOuter])) vars
              -- TODO this is not unique in the case of multiple recursive binding groups in same scope
              -- And deduplicating would also be incorrect for overridden idents
              ctxRef = [ EVarBind lazyVarName $ qualFunCall "erlang" "make_ref" [] | needLazyRef ]

          let s1 = \v -> foldr EAndThen v funs
          let s2 = \v -> foldr ELet v rebinds
          let s3 innermost = case ctxRef of
                [] -> innermost
                [c] -> ELet c innermost

          pure $ \innermost -> s3 (s1 (s2 innermost))


    qualifiedToVar (Qualified _ ident) = identToVar ident

    qualifiedToTypeclassCtor :: Qualified Ident -> Atom
    qualifiedToTypeclassCtor (Qualified (P.ByModuleName mn') ident)
      -- this case could be always qualified, but is not to assist optimisation
      | mn == mn' =
        Atom Nothing (runIdent' ident)
      | otherwise =
        Atom (Just $ atomModuleName mn' PureScriptModule) (runIdent' ident)
    qualifiedToTypeclassCtor (Qualified (P.BySourcePos  _) ident) = Atom Nothing (runIdent' ident)

    valueToErl :: Expr Ann -> m Erl
    valueToErl e = valueToErl' Nothing e

    valueToErl' :: Maybe Ident -> Expr Ann -> m Erl
    valueToErl' mIdent e =
      let (ss,_,_) =  extractAnn e in
      valueToErl'' ss mIdent e

    valueToErl2 :: SourceSpan -> Maybe Ident -> Expr Ann -> m Erl
    valueToErl2 pss mIdent e =
      let
          (ss,_,_) = extractAnn e
          ss2 = case ss of
            SS.NullSourceSpan -> pss
            _ -> ss
      in
      valueToErl'' ss2 mIdent e

    valueToErl'' :: SourceSpan -> Maybe Ident -> Expr Ann -> m Erl
    valueToErl'' ann _ (Literal (pos, _, _) l) =
      rethrowWithPosition pos $ literalToValueErl l
    valueToErl'' ann _ (Var _ (Qualified (P.ByModuleName C.M_Prim) (Ident undef)))
      | undef == C.S_undefined =
        return $ EAtomLiteral $ Atom Nothing C.S_undefined
    valueToErl'' ann _ (Var (_, _, Just (IsConstructor _ [])) (Qualified (P.ByModuleName (ModuleName tipeModu)) ident)) =
      return $ constructorLiteral tipeModu (runIdent' ident) []
    valueToErl'' ann _ (Var _ ident) | isTopLevelBinding ident = pure $
      case M.lookup ident arities of
        Just (Arity (0, 1)) -> EFunRef (qualifiedToErl mn ident) 1
        _
          | Just (EffFnXArity arity) <- effFnArity ident,
            arity > 0 ->
            EFunRef (qualifiedToErl mn ident) arity
        _
          | Just (FnXArity arity) <- fnArity ident,
            arity > 0 ->
            EFunRef (qualifiedToErl mn ident) arity
        _ -> EApp RegularApp (EAtomLiteral $ qualifiedToErl mn ident) []
    valueToErl'' ann _ (Var _ ident) = return $ EVar $ qualifiedToVar ident
    valueToErl'' ann ident (Abs _ arg val) = do
      ret <- valueToErl2 ann Nothing val

      -- TODO this is mangled in corefn json
      let fixIdent (Ident "$__unused") = UnusedIdent
          fixIdent x = x
          arg' = case fixIdent arg of
            UnusedIdent -> "_"
            _ -> identToVar arg
      return $ EFun1 (fmap identToVar ident) arg' ret
    valueToErl'' ann _ (Accessor _ prop val) = do
      eval <- valueToErl2 ann Nothing val
      return $ EApp RegularApp (EAtomLiteral $ Atom (Just "maps") "get") [EAtomLiteral $ AtomPS Nothing prop, eval]
    valueToErl'' ann _ (ObjectUpdate _ o _mstring ps) = do
      obj <- valueToErl2 ann Nothing o
      sts <- mapM (sndM (valueToErl2 ann Nothing)) ps
      return $ EMapUpdate obj (map (first (EAtomLiteral . AtomPS Nothing)) sts)
    valueToErl'' ann _ e@(App (_, _, meta) _ _) = do
      let (f, args) = unApp e []
          eMeta = case meta of
                          Just IsSyntheticApp -> SyntheticApp
                          _ -> RegularApp
      args' <- mapM (valueToErl2 ann Nothing) args
      case f of
        Var (_,_,Just (IsConstructor _ _)) (Qualified (P.ByModuleName (ModuleName "Atom")) (Ident "Atom")) | ([arg]) <- args' ->
          pure $ EApp RegularApp (EAtomLiteral (Atom (Just "erlang") "binary_to_atom")) [arg, EAtomLiteral (Atom Nothing "utf8")]
        Var (_, _, Just IsNewtype) _ ->
          return $ head args'
        Var (_, _, Just (IsConstructor _ fields)) (Qualified (P.ByModuleName (ModuleName tipeModu)) ident)
          | length args == length fields ->
            return $ constructorLiteral tipeModu (runIdent' ident) args'
        Var (_, _, Just IsTypeClassConstructor) name -> do
          let res = curriedApp args' $ EApp eMeta (EAtomLiteral $ qualifiedToTypeclassCtor name) []

              replaceQualifiedSelfCalls = \case
                (EAtomLiteral (Atom (Just emod) x)) | emod == atomModuleName mn PureScriptModule -> EAtomLiteral (Atom Nothing x)
                other -> other
          pure $ everywhereOnErl replaceQualifiedSelfCalls res

        -- fully saturated call to foreign import
        Var _ (Qualified _ ident)
          | isFullySaturatedForeignCall mn actualForeignArities f args ->
            return $ EApp eMeta (EAtomLiteral $ qualifiedToErl' mn ForeignModule ident) args'
        -- Fully saturated (consuming tc dicts and value args in 1 call) (including "over-saturated")
        Var _ qi@(Qualified _ _)
          | Just (Arity (n, m)) <- M.lookup qi arities,
            let arity = n + m,
            length args >= arity ->
            return $ curriedApp (drop arity args') $ EApp eMeta (EAtomLiteral (qualifiedToErl mn qi)) (take arity args')
        -- partially saturated application (all tc dicts to be applied in 1 call and maybe more to be applied to the curried result)
        Var _ qi@(Qualified _ _)
          | Just (Arity (n, _)) <- M.lookup qi arities,
            length args >= n,
            n > 0 ->
            return $ curriedApp (drop n args') $ EApp eMeta (EAtomLiteral (qualifiedToErl mn qi)) (take n args')
        _ -> curriedApp args' <$> valueToErl2 ann Nothing f
      where
        unApp :: Expr Ann -> [Expr Ann] -> (Expr Ann, [Expr Ann])
        unApp (App _ val arg) args = unApp val (arg : args)
        unApp other args = (other, args)


--    valueToErl' _ _ (Case _ values binders) = do
--      vals <- mapM valueToErl2 ann Nothing values
--      (exprs, binders', newvals) <- bindersToErl vals binders
--      -- let ret = EApp (EFunFull (Just "Case") binders') (vals++newvals)
--      let funBinderToBinder = \case
--            (EFunBinder [e] Nothing, ee) -> (EBinder e, ee)
--            (EFunBinder [e] (Just g), ee) -> (EGuardedBinder e g, ee)
--            (EFunBinder es Nothing, ee) -> (EBinder (ETupleLiteral es), ee)
--            (EFunBinder es (Just g), ee) -> (EGuardedBinder (ETupleLiteral es) g, ee)
--      let ret = case (vals, newvals) of
--            ([val'], []) -> ECaseOf val' (map funBinderToBinder binders')
--            (_, []) -> ECaseOf (ETupleLiteral vals) (map funBinderToBinder binders')
--            _ -> EApp RegularApp (EFunFull Nothing binders') (vals ++ newvals)
--      pure $ letbind ELet exprs ret

    -- TODO[drathier]: first arg (Maybe Ident) is passed in sometimes, but never used. Remove it from the function args.

    valueToErl'' ann _ (Case _ values branches) = do
      renamedValues <-
        mapM
          (\v -> do
            v2 <- valueToErl2 ann Nothing v
            case v2 of
              EVar v2n -> pure (v2n, v2)
              _ -> (,) <$> freshNameErl' "CaseOf" <*> pure v2
          )
          values
      (res, resDB) <-
        runStateT
          (caseToErlImpl (map fst renamedValues) branches)
          (DB M.empty [] [] (map fst renamedValues))
      pure
        ( letbindVars ELet renamedValues $
          letbind (\(k,v) rest -> ELet (EBind (EVar k) (EFun0 Nothing v)) rest) (reverse (contImpls resDB)) $
          res
        )
        where
          -- NOTE[drathier]: hash continuations and bind them as local funs, so we don't duplicate code on deeply nested branches
          caseToErlImpl
            :: [T.Text]
            -> [CaseAlternative Ann]
            -> StateT DB m Erl
          caseToErlImpl values branches = do
            branches2 <- branchesToErl branches
            pure $ ECaseOf (tupleWrap (map EVar values)) branches2

          tupleWrap things =
            case things of
              [a] -> a
              _ -> ETupleLiteral things

          buildCont :: [CaseAlternative Ann] -> StateT DB m (Maybe T.Text)
          buildCont [] = pure Nothing
          buildCont branches = do
            db <- get
            case M.lookup branches (conts db) of
              Just contName -> pure (Just contName)
              Nothing -> do
                -- [drathier]: insert name before generating the branch, to avoid duplicate work, if that's even an issue
                contName <- lift (freshNameErl' "Cont")
                put ( db { conts = M.insert branches contName (conts db) } )

                rest <- caseToErlImpl (topmostValues db) branches
                db <- get
                put ( db { contImpls = (contName, rest) : contImpls db } )
                pure (Just contName)

          pushArrayCont :: P.Text -> [Binder Ann] -> StateT DB m ()
          pushArrayCont var binders = do
            db <- get
            put (db {guardConts = ArrayGuard var binders:guardConts db})
            pure ()

          pushSimpleGuard :: Erl -> Erl -> StateT DB m ()
          pushSimpleGuard var binder = do
            db <- get
            put (db {guardConts = SimpleGuard var binder:guardConts db})
            pure ()

          takeConts :: StateT DB m [GuardCont]
          takeConts = do
            db <- get
            put (db {guardConts = []})
            pure (guardConts db)

          branchesToErl :: [CaseAlternative Ann] -> StateT DB m [(EBinder, Erl)]
          branchesToErl branches =
            case branches of
              [] -> do
                pure []
              (CaseAlternative binders mguard):restBranches ->
                case mguard of
                  Right rhs -> do
                    binders2 <- mapM binderToErl binders
                    guardConts <- takeConts
                    rhs2 <- lift $ valueToErl2 ann Nothing rhs
                    case guardConts of
                      [] ->
                        ((EBinder (tupleWrap binders2), rhs2):) <$> branchesToErl restBranches
                      _ -> do
                        monGuardFailureCont <- buildCont restBranches
                        guardsToErl2 <- arrayGuardsToErl monGuardFailureCont guardConts [(ETrue, rhs2)]
                        pure $
                          [ (EBinder (tupleWrap binders2), guardsToErl2) ]
                          <>
                          case monGuardFailureCont of
                            Just onGuardFailureCont ->
                              [ (EBinder (EVar "_"), EApp RegularApp (EVar onGuardFailureCont) []) ]
                            Nothing ->
                              []

                  Left guardedExprs -> do
                    -- we either walk down a nested path of cases to evaluate guards, or we continue down to the next branch
                    binders2 <- mapM binderToErl binders
                    guardConts <- takeConts
                    guardedExprs2 <- mapM (\(g,h) -> (,) <$> lift (valueToErl2 ann Nothing g) <*> lift (valueToErl2 ann Nothing h)) guardedExprs
                    monGuardFailureCont <- buildCont restBranches
                    guardsToErl2 <- arrayGuardsToErl monGuardFailureCont guardConts guardedExprs2
                    pure $
                      [(EBinder (tupleWrap binders2), guardsToErl2)]
                      <>
                      case monGuardFailureCont of
                        Just onGuardFailureCont ->
                          [ (EBinder (EVar "_"), EApp RegularApp (EVar onGuardFailureCont) [])
                          ]
                        Nothing ->
                          []


          arrayGuardsToErl :: Maybe T.Text -> [GuardCont] -> [(Erl, Erl)]-> StateT DB m Erl
          arrayGuardsToErl monGuardFailureCont arrayGuards normalGuards =
            case arrayGuards of
              [] -> guardsToErl monGuardFailureCont normalGuards
              (ArrayGuard arrayPattern binders:restGuards) -> do
                binders2 <- mapM binderToErl binders
                guardConts <- takeConts
                restGuards2 <- arrayGuardsToErl monGuardFailureCont (guardConts <> restGuards) normalGuards
                let contBranch =
                      case monGuardFailureCont of
                        Just onGuardFailureCont ->
                          [ ( EBinder (EVar "_")
                            , EApp RegularApp (EVar onGuardFailureCont) []
                            )
                          ]
                        Nothing ->
                          []
                pure $
                  ECaseOf (EApp RegularApp (EAtomLiteral (AtomPS (Just "array") "size")) [EVar arrayPattern])
                    ([ ( EBinder (ENumericLiteral (Left (toInteger (length binders))))
                      , ECaseOf
                          (EApp RegularApp (EAtomLiteral (AtomPS (Just "array") "to_list")) [EVar arrayPattern])
                          ([ ( EBinder $ EListLiteral binders2
                            , restGuards2
                            )
                          ]
                          <> contBranch
                          )
                      )
                    ]
                    <> contBranch
                    )

              (SimpleGuard cond binder:restGuards) -> do
                guardConts <- takeConts
                restGuards2 <- arrayGuardsToErl monGuardFailureCont (guardConts <> restGuards) normalGuards
                let contBranch =
                      case monGuardFailureCont of
                        Just onGuardFailureCont ->
                          [ ( EBinder (EVar "_")
                            , EApp RegularApp (EVar onGuardFailureCont) []
                            )
                          ]
                        Nothing ->
                          []
                pure $
                  ECaseOf cond
                    ( [ ( EBinder binder
                        , restGuards2
                        )
                      ]
                      <> contBranch
                    )

          guardsToErl :: Maybe T.Text -> [(Erl, Erl)] -> StateT DB m Erl
          guardsToErl Nothing [] = pure $ EVar "Unreachable-drathier-unreachble-empty-guards-without-continuation"
          guardsToErl (Just onGuardFailureCont) [] = pure $ EApp RegularApp (EVar onGuardFailureCont) []
          guardsToErl monGuardFailureCont ((guard, happy):restGuards) = do
            restGuards2 <- guardsToErl monGuardFailureCont restGuards
            pure $
              case guard == ETrue of
                True -> happy
                False ->
                  ECaseOf guard $
                    [ ( EBinder (ETrue)
                      , happy
                      )
                    ]
                    <>
                    case (restGuards, monGuardFailureCont) of
                      ([], Nothing) -> []
                      _ ->
                        [ ( EBinder (EFalse)
                          , restGuards2
                          )
                        ]

          binderToErl :: Binder Ann -> StateT DB m Erl
          binderToErl binder =
            case binder of
              NullBinder _ -> pure $ EVar "_"
              VarBinder _ name -> pure $ EVar (identToVar name)
              ConstructorBinder (_, _, Just (IsConstructor _ _)) (Qualified _ (ProperName "Atom")) (Qualified (P.ByModuleName (ModuleName "Atom")) (ProperName "Atom")) [v] ->
                do
                  ev <- binderToErl v
                  case ev of
                    EStringLiteral s ->
                      pure (EAtomLiteral (AtomPS Nothing s))
                    _ -> do
                      atomVar <- freshNameErl' "AtomLiteral"
                      pushSimpleGuard (EApp RegularApp (EAtomLiteral (Atom (Just "erlang") "atom_to_binary")) [EVar atomVar, EAtomLiteral (Atom Nothing "utf8")]) ev
                      pure (EVar atomVar)
              ConstructorBinder (_, _, Just IsNewtype) _ _ [binder] -> binderToErl binder
              -- ConstructorBinder _ _ (Qualified (P.ByModuleName (ModuleName tipeModu)) (ProperName ctorName)) binders -> do
              ConstructorBinder _ (Qualified _ (ProperName tipeName)) (Qualified (P.ByModuleName (ModuleName tipeModu)) (ProperName ctorName)) binders ->
                handleCtorBinder [] binder
              NamedBinder _ alias binder -> do
                binder2 <- binderToErl binder
                pure (EBind (EVar (identToVar alias)) binder2)
              LiteralBinder _ lit ->
                case lit of
                  NumericLiteral (Left int) -> pure $ ENumericLiteral (Left int)
                  NumericLiteral (Right double) -> pure $ ENumericLiteral (Right double)
                  StringLiteral psString -> pure $ EStringLiteral psString
                  CharLiteral char -> pure $ ECharLiteral char
                  BooleanLiteral True -> pure $ ETrue
                  BooleanLiteral False -> pure $ EFalse
                  ObjectLiteral kvPairs ->
                    EMapPattern <$> mapM (\(k,v) -> (EAtomLiteral $ AtomPS Nothing k,) <$> binderToErl v) kvPairs
                  ArrayLiteral items -> do
                    arrayVar <- freshNameErl' "ArrayLiteral"
                    pushArrayCont arrayVar items
                    pure (EVar arrayVar)

          handleCtorBinder acc binder =
            case binder of
              NullBinder _ ->
                -- base case ignoring result
                pure (EMapPattern (reverse acc))

              ConstructorBinder _ (Qualified _ (ProperName "Map")) (Qualified (P.ByModuleName (ModuleName "Map")) (ProperName "MEmpty")) [] ->
                -- base case empty result
                do
                  mapVar <- freshNameErl' "MapLiteralB"
                  pushSimpleGuard (EApp RegularApp (EAtomLiteral (Atom (Just "maps") "size")) [EVar mapVar]) (ENumericLiteral (Left (toInteger (length acc))))
                  pure (EBind (EMapPattern (reverse acc)) (EVar mapVar))

              ConstructorBinder _ (Qualified _ (ProperName "Map")) (Qualified (P.ByModuleName (ModuleName "Map")) (ProperName "MCons")) [k, v, contBinder] ->
                do
                  ek <- binderToErl k
                  ev <- binderToErl v
                  handleCtorBinder ((ek, ev):acc) contBinder

              ConstructorBinder _ (Qualified _ (ProperName tipeName)) (Qualified (P.ByModuleName (ModuleName tipeModu)) (ProperName ctorName)) binders ->
                do
                  binders2 <- mapM binderToErl binders
                  pure (constructorLiteral tipeModu ctorName binders2)

              _ ->
                do
                  mapVar <- freshNameErl' "MapLiteralA"
                  b2 <- binderToErl binder
                  pushSimpleGuard (EVar mapVar) b2
                  pure (EBind (EMapPattern (reverse acc)) (EVar mapVar))

    valueToErl'' ann _ (Let _ ds val) = do
      ds2 <- mapM bindToErl ds
      ret <- valueToErl2 ann Nothing val
      let ds3 = foldr ($) ret ds2
      pure ds3

    valueToErl'' ann _ (Constructor (_, _, Just IsNewtype) _ _ _) = error "newtype ctor"
    valueToErl'' ann _ (Constructor _ (ProperName tipe) (ProperName ctor) fields) =
      let createFn =
            let body = constructorLiteral tipe ctor ((EVar . identToVar) `map` fields)
             in foldr (EFun1 Nothing . identToVar) body fields
       in pure createFn

    constructorLiteral tipeOrModuleName name args =
      case (tipeOrModuleName, name) of
        ("Atom", "Atom") ->
          case args of
            [a] -> EApp RegularApp (EAtomLiteral (Atom (Just "erlang") "binary_to_atom")) [a, EAtomLiteral (Atom Nothing "utf8")]
        ("List", "Nil") -> EListLiteral []
        ("List", "Cons") -> let [a,ax] = args in EListCons [a] ax
        ("Map", "MEmpty") -> EMapLiteral []
        ("Map", "MCons") ->
          case args of
            [k,v,EMapUpdate base prev] -> EMapUpdate base ((k,v):prev)
            --
            [k,v,EMapLiteral prev] -> EMapLiteral ((k,v):prev)
            [k,v,base] -> EMapUpdate base [(k,v)]
            _ -> error (show ("args", args))
        _ -> ETupleLiteral (EAtomLiteral (Atom Nothing (toAtomName name)) : args)

    literalToValueErl :: Literal (Expr Ann) -> m Erl
    literalToValueErl = fmap fst . literalToValueErl' EMapLiteral (\x -> (,[]) <$> valueToErl x)

    literalToValueErl' :: Show a => ([(Erl, Erl)] -> Erl) -> (a -> m (Erl, [b])) -> Literal a -> m (Erl, [b])
    literalToValueErl' _ _ (NumericLiteral n) = pure (ENumericLiteral n, [])
    literalToValueErl' _ _ (StringLiteral s) = pure (EStringLiteral s, [])
    literalToValueErl' _ _ (CharLiteral c) = pure (ECharLiteral c , [])
    literalToValueErl' _ _ (BooleanLiteral True) = pure (ETrue, [])
    literalToValueErl' _ _ (BooleanLiteral False) = pure (EFalse, [])
    literalToValueErl' _ f (ArrayLiteral xs) = do
      args <- mapM f xs
      let binds = snd <$> args
      pure (EArrayLiteral (map fst args), concat binds)
    literalToValueErl' mapLiteral f (ObjectLiteral ps) = do
      pairs <- mapM (sndM f) ps
      pure (mapLiteral $ map (\(label, (e, _)) -> (EAtomLiteral $ AtomPS Nothing label, e)) pairs, concatMap (snd . snd) pairs)
{-
    bindersToErl :: [Erl] -> [CaseAlternative Ann] -> m ([Erl], [(EFunBinder, Erl)], [Erl])
    bindersToErl vals cases = do
      let binderLengths = map (length . caseAlternativeBinders) cases
          maxBinders = maximum binderLengths
      if length (nub binderLengths) > 1
        then traceM $ "Found inconsistent binder lengths: " <> show binderLengths
        else pure ()
      res <- mapM (caseToErl maxBinders) cases
      let arrayVars = map fst $ concatMap (\(_, _, x) -> x) res
          convBinder (count, binds) (_, binders, arrayMatches) =
            (count + length arrayMatches, binds ++ map go binders)
            where
              go (EFunBinder bs, e) = (EFunBinder (bs ++ padBinds count arrayMatches), e)
          padBinds n binds = replicate n (EVar "_") ++ map snd binds ++ replicate (length arrayVars - n - length binds) (EVar "_")
          binders' = snd $ foldl convBinder (0, []) res

      pure (concatMap (\(x, _, _) -> x) res, binders', arrayVars)
      where
        caseToErl :: Int -> CaseAlternative Ann -> m ([Erl], [(EFunBinder, Erl)], [(Erl, Erl)])
        caseToErl numBinders (CaseAlternative binders alt) = do
          let binders' = binders ++ replicate (numBinders - length binders) (NullBinder (nullSourceSpan, [], Nothing))
              vars = nub $ concatMap binderVars binders'

          newVars <- map Ident <$> replicateM (length vars) (freshNameErl' "CaseToErl")

          -- TODO we replace because case expressions do not introduce a scope for the binders, but to preserve identifier
          -- names we could do so only for those identifiers which are not already fresh here
          -- but we currently don't have a parent scope available
          let (_, replaceExpVars, replaceBinderVars) = everywhereOnValues id id id -- (replaceEVars (zip vars newVars)) (replaceBVars (zip vars newVars))

          let binders'' = map replaceBinderVars binders'
              alt' = case replaceExpVars (Case (nullSourceSpan, [], Nothing) [] [CaseAlternative [] alt]) of
                        Case _ [] [CaseAlternative [] alt'] -> alt'
                        _ -> internalError "Replacing variables should give the same form back"


          (bs, binderContext) :: ([Erl], [((EFunBinder, [Erl]) -> Erl, (T.Text, Erl))]) <- second concat . unzip <$> mapM binderToErl' binders''

          -- binderContext is bindings required to make pattern matching work in binders, ie converting arrays to lists
          -- let (binderBinds, binderVars) = map ($ EFunBinder bs Nothing) *** map (first EVar) $ unzip binderContext

          let contextStep (binds, bindVars) (mkBind, otherContext) =
                (binds <> [ mkBind (EFunBinder (bs <> map snd bindVars), vals ++ map fst bindVars) ], bindVars <> [ first EVar otherContext ])
              (binderBinds, binderVars) = foldl' contextStep ([], []) binderContext


          (es, res) <- case alt' of
            Right e -> do
              e' <- valueToErl2 ann Nothing e
              pure ([], [(EFunBinder bs, e')])
            Left guards -> first concat . unzip <$> mapM (guardToErl bs) guards

          pure (es ++ binderBinds, res, binderVars)

        mapMIndex :: Monad m => (Int -> a -> m b) -> [a] -> m [b]
        mapMIndex f xs = sequence $ zipWith f [0..] xs

        guardToErl :: [Erl] -> (Expr Ann, Expr Ann) -> m ([Erl], (EFunBinder, Erl))
        guardToErl bs (ge, e) = do
          var <- freshNameErl' "GuardVar"
          ge' <- valueToErl2 ann Nothing ge
          let binder = EFunBinder bs
              fun =
                EFunFull
                  Nothing
                  ( (binder, ge') :
                      [(EFunBinder (replicate (length bs) (EVar "_")), EFalse) | not (irrefutable binder)]
                  )
              cas = EApp RegularApp fun vals
          e' <- valueToErl2 ann Nothing e
          pure ([EVarBind var cas], (EFunBinder bs (Just $ Guard $ EVar var), e'))
-}
    binderVars :: Binder Ann -> [Ident]
    binderVars (VarBinder _ ident) = [ident]
    binderVars (NamedBinder _ ident binder) = ident : binderVars binder
    binderVars (LiteralBinder _ (ArrayLiteral es)) = concatMap binderVars es
    binderVars (LiteralBinder _ (ObjectLiteral fields)) = concatMap (binderVars . snd) fields
    binderVars (LiteralBinder _ _) = []
    binderVars (ConstructorBinder _ _ _ binders) = concatMap binderVars binders
    binderVars (NullBinder _) = []

    replaceEVars :: [(Ident, Ident)] -> Expr Ann -> Expr Ann
    replaceEVars vars (Var a (Qualified q@(P.BySourcePos  _) x)) | x `notElem` topLevelNames = Var a $ Qualified q $ fromMaybe x $ lookup x vars
    replaceEVars _ z = z

    replaceBVars :: [(Ident, Ident)] -> Binder Ann -> Binder Ann
    replaceBVars vars (VarBinder a x) = VarBinder a $ fromMaybe x $ lookup x vars
    replaceBVars vars (NamedBinder a x b) = NamedBinder a (fromMaybe x $ lookup x vars) b
    replaceBVars _ z = z

    binderToErl' :: Binder Ann -> m (Erl, [((EFunBinder, [Erl]) -> Erl, (T.Text, Erl))])
    binderToErl' (NullBinder _) = pure (EVar "_", [])
    binderToErl' (VarBinder _ ident) = pure (EVar $ identToVar ident, [])
    binderToErl' (LiteralBinder _ (ArrayLiteral es)) = do
      x <- freshNameErl' "ArrayLiteralBinderX"
      args' <- mapM binderToErl' es

      let arraySize = EAtomLiteral $ Atom (Just "array") "size"
          arrayToList = EAtomLiteral $ Atom (Just "array") "to_list"
          cas (binder, vals) =
            EApp RegularApp
              ( EFunFull
                  Nothing
                  ( (binder
                  , ECaseOf (EApp RegularApp arraySize [EVar x])
                      [ (EBinder (ENumericLiteral (Left (toInteger (length es)))), EApp RegularApp arrayToList [EVar x])
                      , (EBinder (EVar "_"), EAtomLiteral (Atom Nothing "array_was_wrong_size"))
                      ]
                  ) :
                      [(EFunBinder (replicate (length vals) (EVar "_")), EAtomLiteral $ Atom Nothing "fail") | not (irrefutable binder)]
                  )
              )
              vals
      var <- freshNameErl' "ArrayLiteralBinderVar"

      let arr = EListLiteral (map fst args')
      pure (EVar x, (EVarBind var . cas, (var, arr)) : concatMap snd args')

    irrefutable (EFunBinder bindEs) = all isOk bindEs
      where
        isOk (EVarBind _ e) = isOk e
        isOk (EVar _) = True
        -- NOTE[drathier]: the below patterns are irrefutable iff the typechecker has checked this case, and perhaps it might not have here.
        -- isOk (EMapLiteral fields) = all isOk (map snd fields)
        -- isOk (ETupleLiteral fields) = all isOk fields
        isOk _ = False
    -- irrefutable _ = False

    ensureFreshVars_ = ensureFreshVars Set.empty M.empty

    ensureFreshVars :: Set Ident -> Map Ident Ident -> Expr Ann -> m (Expr Ann)
    ensureFreshVars scope vars = go
      where
        go :: Expr Ann -> m (Expr Ann)
        go expr = case expr of
          Literal ann lit ->
            Literal ann <$> case lit of
              ArrayLiteral exs -> ArrayLiteral <$> traverse go exs
              ObjectLiteral fields -> ObjectLiteral <$> traverse (traverse go) fields
              _ -> pure lit
          Constructor {} -> pure expr
          Accessor ann ps ex -> Accessor ann ps <$> go ex
          ObjectUpdate ann e mstring updates -> ObjectUpdate ann <$> go e <*> pure mstring <*> traverse (traverse go) updates
          Abs ann ident e -> do
            (ident', scope', vars') <- bindIdent scope vars ident
            Abs ann ident' <$> ensureFreshVars scope' vars' e
          App ann e1 e2 -> App ann <$> go e1 <*> go e2
          (Var a (Qualified q@(P.BySourcePos  _) x)) | x `notElem` topLevelNames -> pure $ Var a $ Qualified q $ fromMaybe x $ M.lookup x vars
          otherVar@Var {} -> pure otherVar
          Case ann es cases -> Case ann <$> traverse go es <*> traverse goCase cases
          Let ann binds e -> do
            (scope', vars', binds') <- foldM goBind (scope, vars, []) binds
            Let ann (reverse binds') <$> ensureFreshVars scope' vars' e

        bindIdent scope vars = \case
          ident@UnusedIdent -> pure (ident, scope, vars)
          Ident "$__unused" -> pure (UnusedIdent, scope, vars)
          ident@P.GenIdent {} -> pure (ident, scope, vars)
          ident@P.InternalIdent {} -> pure (ident, scope, vars)
          ident@(Ident rawIdent) ->
            if lowerIdent ident `Set.member` scope
              then do
                newIdent <- Ident <$> freshNameErl' rawIdent
                pure (newIdent, Set.insert (lowerIdent newIdent) scope, M.insert ident newIdent vars)
              else pure (ident, Set.insert (lowerIdent ident) scope, vars)
          where
            lowerIdent (Ident x) = Ident $ T.toLower x
            lowerIdent other = other

        goBind :: (Set Ident, Map Ident Ident, [Bind Ann]) -> Bind Ann -> m (Set Ident, Map Ident Ident, [Bind Ann])
        goBind (scope, vars, acc) = \case
          NonRec ann ident e -> do
            (ident', scope', vars') <- bindIdent scope vars ident
            e' <- ensureFreshVars scope' vars' e
            pure (scope', vars', NonRec ann ident' e' : acc)
          Rec binds -> do
            let idents = snd . fst <$> binds
            (idents', scope', vars') <-
              foldM
                ( \(accIdents, accScope, accVars) ident -> do
                    (ident', scope', vars') <- bindIdent accScope accVars ident
                    pure (ident' : accIdents, scope', vars')
                )
                ([], scope, vars)
                idents
            let f (newIdent, ((ann, _oldIdent), e)) = ((ann, newIdent),) <$> ensureFreshVars scope' vars' e
            binds' <- traverse f $ zip (reverse idents') binds
            pure (scope', vars', Rec binds' : acc)

        goCase (CaseAlternative ann (Right e)) =
          CaseAlternative ann . Right <$> go e
        goCase (CaseAlternative ann (Left ges)) =
          CaseAlternative ann . Left
            <$> traverse (traverse go) ges

letbindVars elet exprs innermost =
  case exprs of
    [] -> innermost
    (name,e):es ->
      case EVar name == e of
        True -> letbindVars elet es innermost
        False -> elet (EBind (EVar name) e) (letbindVars elet es innermost)

letbind elet exprs innermost =
  case exprs of
    [] -> innermost
    e:es -> elet e (letbind elet es innermost)


mapMK :: Monad m => ((Erl -> Erl) -> a -> m (b, Erl -> Erl)) -> (Erl -> Erl) -> [a] -> m ([b], Erl -> Erl)
mapMK f kont values =
  case values of
    [] -> pure ([], kont)
    v:vs -> do
      (v', kont') <- f kont v
      (res, kont3) <- mapMK f kont' vs
      pure (v':res, kont3)

funBinderToBinder = \case
  (EFunBinder [e], ee) -> (EBinder e, ee)
  -- (EFunBinder [e] (Just g), ee) -> (EGuardedBinder e g, ee)
  (EFunBinder es, ee) -> (EBinder (ETupleLiteral es), ee)
  -- (EFunBinder es (Just g), ee) -> (EGuardedBinder (ETupleLiteral es) g, ee)

data GuardCont
  = ArrayGuard T.Text [Binder Ann]
  | SimpleGuard Erl Erl

--letbindM :: (Erl -> Bind Ann -> m Erl) -> [Bind Ann] -> Erl -> m Erl
--letbindM runBind binds innermost =
--  case binds of
--    [] -> pure innermost
--    e:es -> do
--      b <- runBind e b
--      rest <- letbind elet es innermost
--letbindM :: (Erl -> Bind Ann -> m Erl) -> [Bind Ann] -> Erl -> m Erl
--letbindM runBind binds innermost =
--  case binds of
--    [] -> pure innermost
--    e:es -> do
--      b <- runBind e b
--      rest <- letbind elet es innermost
--letbindM :: (Erl -> Erl -> Erl) -> (Erl -> Bind Ann -> m Erl) -> [Bind Ann] -> Erl -> m Erl
--letbindM elet runBind exprs innermost =
--  case exprs of
--    [] -> pure innermost
--    e:es -> do
--      elet2 <- elet e
--      rest <- letbind elet es innermost


-- NOTE[drathier]: hash continuations and bind them as local funs, so we don't duplicate code on deeply nested branches
data DB
  = DB
    { conts :: M.Map [CaseAlternative Ann] T.Text
    , contImpls :: [(T.Text, Erl)]
    , guardConts :: [GuardCont]
    , topmostValues :: [T.Text]
    }
