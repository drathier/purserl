module Language.PureScript.CoreFn.Optimizer (optimizeCoreFn) where

import Protolude hiding (Type, moduleName, traceM)
import Prelude (error)

import Control.Monad.Supply (Supply)
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.CSE (optimizeCommonSubexpressions)
import Language.PureScript.CoreFn.Expr (Bind(..), Expr(..), CaseAlternative(..), bindIdents)
import Language.PureScript.CoreFn.Module (Module(..))
import Language.PureScript.CoreFn.Traversals (everywhereOnValues, traverseCoreFn, traverseCoreFnFull)
import Language.PureScript.Constants.Libs qualified as C
import Debug.Trace qualified as Debug
import System.IO.Unsafe
import Language.PureScript.Names (Ident(..), runIdent, ModuleName, QualifiedBy(..), runModuleName)
import Control.DeepSeq (force)
import Control.Monad.Trans.RWS.Strict (evalRWST, asks, local, RWST)
import Control.Monad.State
import Data.Text (Text, append, pack)
import Debug.Trace (traceM)
import Control.Monad.State
import Data.Text (Text)
import qualified Data.Map as Map
import Data.Map (Map)
import Language.PureScript.CoreFn.Binders (Binder(..))
import Data.Text qualified as T
import Debug.Trace qualified as Debug

import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.CoreFn.Binders (Binder(..))
import Language.PureScript.CoreFn.Expr (Bind(..), CaseAlternative(..), Expr(..))
import Language.PureScript.Names (Ident, ProperName, ProperNameType(..), Qualified(..))


-- |
-- CoreFn optimization pass.
--
optimizeCoreFn :: Module Ann -> Supply (Module Ann)
-- optimizeCoreFn m = pure m
optimizeCoreFn m =
  fmap (\md -> m {moduleDecls = md}) $
  -- Debug.trace (show ("optimizeCoreFn1", "dummy")) $
  optimizeCommonSubexpressions (moduleName m) $
  -- Debug.trace (show ("optimizeCoreFn2", "dummy")) $
  optimizeModuleDecls (moduleName m) (moduleForeign m) $
  -- Debug.trace (show ("optimizeCoreFn3", "dummy")) $
  moduleDecls m

optimizeModuleDecls :: ModuleName -> [Ident] -> [Bind Ann] -> [Bind Ann]
optimizeModuleDecls modu foreignIdents binds =
  map transformBinds $
    force $
      map (
       renameIdentsAndVars modu
        ( -- Debug.trace (show ("renameIdentsAndVars-pre", modu)) $
          Map.fromList $
            map (\v -> (v,())) $
            (concatMap bindIdents binds <>
            foreignIdents
            )
        )
      ) $
      binds

  where
  -- (handleBind, handleExprDefault, handleBinder, _) = traverseCoreFn handleBind handleExpr handleBinder handleCaseAlternative
  -- (uniqueNamedBinds, uniqueExprs, uniqueBinder, uniqueCase) = traverseCoreFn (p uniqueNamedBinds) (p uniqueExprs) (p uniqueBinder) (p uniqueCase)
  (transformBinds, _, _) = everywhereOnValues identity transformExprs identity
  transformExprs
    = optimizeDataFunctionApply

  p f a =
    do
      traceM (show a)
      f a
{-
  printExprs x =
    do
      traceM (show x)
      uniqueExprs case x of
        Let a binds rhs ->
          do
            renamedBinds <- mapM regBind binds
            pure (Let a renamedBinds rhs)
        Abs a ident rhs ->
          do
            rIdent <- renameIdent ident
            pure (Abs a rIdent rhs)
        Var a qual ->
          do
            rqual <- renameVar qual
            pure (Var a rqual)
        Constructor a tname cname idents ->
          do
            ridents <- mapM renameIdent idents
            pure (Constructor a tname cname ridents)
        _ ->
          pure x
  f b =
    unsafePerformIO $ uniqueNamedBinds b
    -- case b of
    --   NonRec _ (Ident "zf") _ ->
    --     unsafePerformIO $
    --       uniqueNamedBinds b
    --   _ ->
    --     uniqueNamedBinds b
    --     b
-}

optimizeDataFunctionApply :: Expr a -> Expr a
optimizeDataFunctionApply e =
  case e of
    (App a (App _ (Var _ fn) x) y)
      | C.I_functionApply <- fn -> App a x y
      | C.I_functionApplyFlipped <- fn -> App a y x
-- {-
    (App a (Var b fn) (Var c impl)) | C.I_map <- fn, C.I_functorArray <- impl -> Var a C.I_arrayMap
    (App a (Var b fn) (Var c impl)) | C.I_map <- fn, C.I_functorMaybe <- impl -> Var a C.I_maybeMap
    (App a (Var b fn) (Var c impl)) | C.I_map <- fn, C.I_functorEither <- impl -> Var a C.I_eitherMap
    -- (App a (Var b fn) (Var c impl)) | C.I_map <- fn, C.I_functorList <- impl -> Var a C.I_listMap -- TODO[drathier]: impl is typeclassy; reimplement without type class or we'll get an inf loop here
    -- (App a (Var b fn) (Var c impl)) | C.I_map <- fn, C.I_functorEffect <- impl -> Var a C.I_eMap -- TODO[drathier]: implementation in E.purs is just Functor.map, so inf loop if we do this rewrite. Effect.purs doesn't expose the function. UnsafePerformEffect doesn't seem pretty enough. Implement it in FFI instead.

    -- TODO[drathier]: Bind takes two args at once sometimes, so blocked on another stab at the exact-call optimization
    -- -- (control_bind@ps:bind((control_bind@ps:bindArray())))
    -- (App a (Var b fn) (Var c impl)) | C.I_bind <- fn, C.I_bindArray <- impl -> Var a C.I_arrayBind
    -- -- (control_bind@ps:bind((maybe@ps:bindMaybe())))
    -- (App a (Var b fn) (Var c impl)) | C.I_bind <- fn, C.I_bindMaybe <- impl -> Var a C.I_bindMaybe
    -- -- (control_bind@ps:bind((either@ps:bindEither())))
    -- (App a (Var b fn) (Var c impl)) | C.I_bind <- fn, C.I_bindEither <- impl -> Var a C.I_eitherBind
    -- (control_bind@ps:bind((effect@ps:bindEffect())))
    -- (App a (Var b fn) (Var c impl)) | C.I_bind <- fn, C.I_bindEffect <- impl -> Var a C.I_effectBindE
    -- (App a (Var b fn) (Var c impl)) | C.I_eBind <- fn, C.I_bindEffect <- impl -> Var a C.I_effectBindE



    -- (control_applicative@ps:pure((control_applicative@ps:applicativeArray())))
    (App a (Var b fn) (Var c impl)) | C.I_pure <- fn, C.I_applicativeArray <- impl -> Var a C.I_arrayPure
    (App a (Var b fn) (Var c impl)) | C.I_pure <- fn, C.I_applicativeMaybe <- impl -> Var a C.I_maybePure
    (App a (Var b fn) (Var c impl)) | C.I_pure <- fn, C.I_applicativeEither <- impl -> Var a C.I_eitherPure
    -- (control_applicative@ps:pure((effect@ps:applicativeEffect())))
    (App a (Var b fn) (Var c impl)) | C.I_pure <- fn, C.I_applicativeEffect <- impl -> Var a C.I_effectPureE

    -- ASSUMPTION[drathier]: all discard instances are implemented as `discard = bind`
    -- (control_bind@ps:discard((control_bind@ps:discardUnit()), (effect@ps:bindEffect())))
    -- [drathier]: this rewrite pattern gave no diff: (App a (App b (Var c fn) (Var d _discardImpl)) (Var e bindImpl)) | C.I_discard <- fn, C.I_bindEffect <- bindImpl -> Var a C.I_effectBindE
    -- (e@ps:discard((control_bind@ps:discardUnit()))) -- wrong discard

    -- [drathier]: these patterns aren't safe, because erl ast optimization translates these to ELet instead of EAndThen, so they might get silently dropped if the're `andthen # (\_ ->` since that might eventually become `let _ =` which then gets dropped. We'll have to move the specialization step here into erl ast instead.
    -- (App a (Var b fn) (Var c _discardImpl)) | C.I_eDiscard <- fn -> Var a C.I_effectBindE
    -- (App a (Var b fn) (Var c impl)) | C.I_discard <- fn, C.I_eDiscard <- impl -> Var a C.I_effectBindE
-- -}
    _ -> e


-------------------------
-- NOTE[drathier]: core idea: traverse ast top-down (in variable binding order), registering variables for renaming as we go without renaming them, and then translating all idents including the ones in e.g. the left side of a let bind. `let a = a+1` registers the `a => a42` mapping (to a unique var), then when it gets to each of the `a`'s, they'll get renamed as idents using the `a => a42` mapping.

-- -- Assuming Ident is defined as:
-- newtype Ident = Ident Text deriving (Show, Eq, Ord)

-- State contains a counter for generating unique names and an environment for scoping
type RenameState = (Int, Map Ident Ident, Map Ident (), Text, Bool)

-- Look up an identifier in the environment, defaulting to the original name
lookupIdent :: Ident -> State RenameState Ident
lookupIdent ident = do
  (_, env, _, prefix, isCtor) <- get
  pure $ case (if isCtor then Just (Ident (prefix <> runIdent ident)) else Nothing) <|> Map.lookup ident env of
    Just v ->
      -- Debug.trace (show ("CoreFn.Optimizer.lookupIdent Map.lookup Just", ident, v, env)) $
      v
    Nothing ->
      -- TODO[drathier]: we'll likely have to either stop returning idents here and only rely on recursion schemes to translate idents after registering them here, or add identity mappings for already translated idents
      -- Debug.trace (show ("CoreFn.Optimizer.lookupIdent Map.lookup Nothing", prefix, ident, env)) $
      ident
  -- pure ident

-- Extend the environment with a new mapping for a bound variable. Returns the input Ident for convenience in <$> <*> chains. The name will be replaced when the inner ident is processed.
extendEnv :: Ident -> State RenameState Ident
extendEnv old = do
  (counter, env, ignores, prefix, isCtor) <- get
  case Map.member old ignores of
    True ->
      -- (\res -> Debug.trace (show ("extendEnv ignored", old, "res", res)) res) <$>
      pure old
    False -> do
      let new = Ident (prefix <> runIdent old <> T.pack ("_" ++ show counter))
      -- (\res -> Debug.trace (show ("extendEnv change", old, counter, new, "res", res)) res) <$>
      do
        -- Debug.traceM (show ("CoreFn.Optimizer.extendEnv", old, new, counter, env))
        put (counter + 1, Map.insert old new env, ignores, prefix, isCtor)
        pure old


renameIdentsAndVars :: forall a. NFData a => Show a => ModuleName -> Map Ident () -> Bind a -> Bind a
renameIdentsAndVars modu topLevelFunctions bind =
  -- (\res -> Debug.trace (show ("renameIdentsAndVars-after", bind, "res", res)) res) $
  let
    isolated :: forall b. State RenameState b -> State RenameState b
    isolated inner = do
      (counter, vars, ignores, prefix, isCtor) <- get
      res <- inner
      (counter2, _, _, _, _) <- get
      put (counter2, vars, ignores, prefix, isCtor)
      pure res

    extendPrefix :: forall b. Ident -> State RenameState b -> State RenameState b
    extendPrefix ident inner = do
      (counter1, vars1, ignores1, prefix1, isCtor1) <- get
      put (counter1, vars1, ignores1, prefix1 <> runIdent ident <> "_", isCtor1)
      res <- inner
      (counter2, vars2, ignores2, _, isCtor2) <- get
      put (counter2, vars2, ignores2, prefix1, isCtor2)
      pure res

    -- Rename identifiers at usage sites
    onIdent :: Ident -> State RenameState Ident
    onIdent ident = do
      -- (\res -> Debug.trace (show ("onIdent", ident, "res", res)) res) <$>
      do
        (_, _, ignores, _, _) <- get
        case Map.member ident ignores of
          True ->
            pure ident
          False ->
            lookupIdent ident

    -- Handle binders in CaseAlternatives
    onCaseAlt :: CaseAlternative a -> State RenameState (CaseAlternative a)
    onCaseAlt caseAlt =
      -- (\res -> Debug.trace (show ("onCaseAlt", caseAlt, "res", res)) res) <$>
      -- traverseCaseAlt caseAlt
      pure caseAlt
      -- case caseAlt of
      --   CaseAlternative binders result -> do
      --     newBinders <- traverse (\binder -> onBinder binder >>= onBinder) binders
      --     let goResult (Left guards) = Left <$> traverse (\(guard, expr) -> (,) <$> onExpr guard <*> onExpr expr) guards
      --         goResult (Right expr) = Right <$> onExpr expr
      --     extendEnv binders newBinders $ CaseAlternative newBinders <$> goResult result

    onBind :: Bind a -> State RenameState (Bind a)
    onBind b =
      -- [drathier]: register new idents, and rely on recursion scheme to rename idents according to the new names right afterwards
      -- (\res -> Debug.trace (show ("onBind", b, "res", res)) res) <$>
      -- traverseBind =<<
        case b of
          NonRec ann ident expr -> do
            extendEnv ident
            pure b
          Rec bindings -> do
            traverse (extendEnv . snd . fst) bindings
            pure b

    onBinder :: Binder a -> State RenameState (Binder a)
    onBinder binder =
      -- (\res -> Debug.trace (show ("onBinder", binder, "res", res)) res) <$>
      -- traverseBinder =<<
        case binder of
          VarBinder ann ident -> do
            extendEnv ident
            pure binder
          NamedBinder ann ident _innerBinder -> do
            extendEnv ident
            pure binder
          _ -> pure binder

    onExpr :: Expr a -> State RenameState (Expr a)
    onExpr e =
      -- (\res -> Debug.trace (show ("onExpr", e, "res", res)) res) <$>
      -- traverseExpr =<<
        case e of
          Abs a ident expr -> Abs a <$> extendEnv ident <*> pure expr
          _ -> pure e

    goBind :: Bind a -> State RenameState (Bind a)
    goBind b =
      onBind b >>=
      \b -> case b of
        NonRec ann ident expr -> do
          NonRec <$> pure ann <*> goIdent ident <*> extendPrefix ident (goExpr expr)
        Rec bindings -> do
          Rec <$>
            traverse (\((ann, ident), expr) -> do
              isolated $ (,) <$> ((,) <$> pure ann <*> goIdent ident) <*> extendPrefix ident (goExpr expr)
            ) bindings

    goExpr :: Expr a -> State RenameState (Expr a)
    goExpr expr =
      onExpr expr >>=
      \expr -> case expr of
        Literal ann lit ->
          Literal <$> pure ann <*> goLitExpr lit
        Constructor ann typeName ctorName fields -> do
          (s@(a,b,c,d,isCtor)) <- get
          put (a,b,c,d,True)
          res <- Constructor <$> pure ann <*> pure typeName <*> pure ctorName <*> traverse goIdent fields
          put (a,b,c,d,isCtor)
          pure res
        Accessor ann prop expr ->
          Accessor <$> pure ann <*> pure prop <*> goExpr expr
        ObjectUpdate ann obj mCopy fields ->
          ObjectUpdate <$> pure ann <*> goExpr obj <*> pure mCopy <*> traverse (\(k, v) -> (,) <$> pure k <*> goExpr v) fields
        Abs ann ident body ->
          Abs <$> pure ann <*> goIdent ident <*> goExpr body
        App ann fn arg ->
          App <$> pure ann <*> goExpr fn <*> goExpr arg
        Var ann qIdent ->
          Var <$> pure ann <*> goQIdent qIdent
        Case ann cases alts ->
          Case <$> pure ann <*> traverse goExpr cases <*> traverse goCaseAlt alts
        Let ann binds body ->
          Let <$> pure ann <*> traverse goBind binds <*> goExpr body

    goQIdent :: Qualified Ident -> State RenameState (Qualified Ident)
    goQIdent qi =
        case qi of
          -- Qualified (BySourcePos spos) a ->
          -- [drathier]: stop recursing on external module, so we don't rename those qidents
          Qualified (ByModuleName qmodu) a | qmodu /= modu -> pure qi
          Qualified qualifiedBy a ->
            -- [drathier]: internal modules are recursed into, and translated. We could translate them here directly, and make the code a bit easier to read
            Qualified <$> pure qualifiedBy <*> goIdent a

    goBinder :: Binder a -> State RenameState (Binder a)
    goBinder b =
      onBinder b >>=
      \b -> case b of
        NullBinder ann ->
          pure $ NullBinder ann
        LiteralBinder ann lit ->
          LiteralBinder <$> pure ann <*> goLitBinder lit
        VarBinder ann ident ->
          VarBinder <$> pure ann <*> goIdent ident
        ConstructorBinder ann qType qCtor binders ->
          ConstructorBinder <$> pure ann <*> pure qType <*> pure qCtor <*> traverse goBinder binders
        NamedBinder ann ident binder ->
          NamedBinder <$> pure ann <*> goIdent ident <*> goBinder binder

    goCaseAlt :: CaseAlternative a -> State RenameState (CaseAlternative a)
    goCaseAlt c =
      isolated $
      onCaseAlt c >>=
      \c -> case c of
        CaseAlternative binders result ->
          CaseAlternative <$> traverse goBinder binders <*> goResult result
            where
              goResult (Left guards) = Left <$> traverse (\(guard, expr) -> (,) <$> goExpr guard <*> goExpr expr) guards
              goResult (Right expr) = Right <$> goExpr expr

    goLitExpr :: Literal (Expr a) -> State RenameState (Literal (Expr a))
    goLitExpr le =
      onLitExpr le >>=
      \le -> case le of
        NumericLiteral n ->
          pure $ NumericLiteral n
        StringLiteral s ->
          pure $ StringLiteral s
        CharLiteral c ->
          pure $ CharLiteral c
        BooleanLiteral b ->
          pure $ BooleanLiteral b
        ArrayLiteral xs ->
          ArrayLiteral <$> traverse goExpr xs
        ObjectLiteral fields ->
          ObjectLiteral <$> traverse (\(k, v) -> (,) <$> pure k <*> goExpr v) fields

    goLitBinder :: Literal (Binder a) -> State RenameState (Literal (Binder a))
    goLitBinder lb =
      onLitBinder lb >>=
      \lb -> case lb of
        NumericLiteral n ->
          pure $ NumericLiteral n
        StringLiteral s ->
          pure $ StringLiteral s
        CharLiteral c ->
          pure $ CharLiteral c
        BooleanLiteral b ->
          pure $ BooleanLiteral b
        ArrayLiteral xs ->
          ArrayLiteral <$> traverse goBinder xs
        ObjectLiteral fields ->
          ObjectLiteral <$> traverse (\(k, v) -> (,) <$> pure k <*> goBinder v) fields

    onLitBinder = pure
    onLitExpr = pure

    goIdent :: Ident -> State RenameState Ident
    goIdent = onIdent
  in
  force $ evalState (goBind bind)--(Debug.trace (show ("renameIdentsAndVars-startbind", modu, bind)) $ bind))
    ( 0
    , Map.empty
    , topLevelFunctions <> Map.fromList (map (\v -> (v,())) (bindIdents bind)) <> unusedVar
    , runModuleName modu <> "_"
    , False
    )

unusedVar =
  -- NOTE[drathier]: this is needed to reduce the number of unique persistent term keys in type class dict memoization. Erlang allows `_` to be rebound (but not _ prefixed vars).
  Map.fromList [(UnusedIdent, ())]
