{-# LANGUAGE ScopedTypeVariables #-}

module Language.PureScript.Erl.CodeGen.LambdaLift (lambdaLift) where

import Prelude.Compat

import Control.Monad (forM)
import Control.Monad.Supply.Class (MonadSupply, fresh)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Control.Monad.Trans (lift)

import Data.List (nub)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T

import Language.PureScript.Erl.CodeGen.AST


lambdaLift :: MonadSupply m => [Erl] -> m [Erl]
lambdaLift decls = do
  (decls', lifted) <- runWriterT (traverse (liftDecl globalScope) decls)
  pure (lifted <> decls')
  where
  -- All top-level function names are globally available; never need capturing.
  globalScope :: Set T.Text
  globalScope = Set.fromList
    [ runAtom name
    | EFunctionDef _ _ name _ _ <- decls
    ]
  runAtom (Atom _ t)    = t
  runAtom (AtomPS _ ps) = T.pack (show ps)  -- fallback, shouldn't occur here


type Lift m = WriterT [Erl] m

liftDecl :: MonadSupply m => Set T.Text -> Erl -> Lift m Erl
liftDecl globalScope (EFunctionDef mty mss name args body) = do
  body' <- liftExpr globalScope (Set.fromList args) body
  pure (EFunctionDef mty mss name args body')
liftDecl _ other = pure other


-- | `globalScope`: module-level names that never need capturing.
-- | `scope`:       variables bound in enclosing lexical scope (for sequential
--                  binding accumulation — used only to correctly recurse, not
--                  for the free-var calculation).
liftExpr :: MonadSupply m => Set T.Text -> Set T.Text -> Erl -> Lift m Erl
liftExpr globalScope scope = go
  where
  go expr = case expr of

    EFunFull mname clauses -> do
      -- Recurse bottom-up, extending scope with each clause's binders.
      clauses' <- forM clauses $ \(EFunBinder pats, body) -> do
        let clauseScope = scope <> Set.fromList (concatMap patVars pats)
        body' <- liftExpr globalScope clauseScope body
        pure (EFunBinder pats, body')

      -- Free vars = vars used in the lambda not bound by its own binders,
      -- minus anything that's a global (module-level function name / atom).
      let freeVs = Set.toAscList $
            freeVarsOfFun clauses' `Set.difference` globalScope

      n <- lift fresh
      let base     = maybe "lambda" id mname
          atomName = Atom Nothing (base <> T.pack (show n))

      let (liftedDef, origArity) = case clauses' of
            [(EFunBinder pats, body)] ->
              let argNames = nub $ freeVs ++ concatMap patVars pats
                  def = EFunctionDef Nothing Nothing atomName argNames body
              in (def, length pats)
            _ ->
              let liftedClauses = map (prependFreeVarPats freeVs) clauses'
                  headerArgs    = nub $ freeVs ++ firstClauseBoundVars clauses'
                  def = EFunctionDef Nothing Nothing atomName headerArgs
                          (EFunFull mname liftedClauses)
              in (def, funArity clauses)

      tell [liftedDef]

      let newArgVars = map (\i -> "_LL" <> T.pack (show n) <> "_" <> T.pack (show i))
                           [0 .. origArity - 1]
          callBody   = EApp RegularApp (EAtomLiteral atomName)
                           (map EVar freeVs ++ map EVar newArgVars)
          wrapper    = EFunN Nothing newArgVars callBody
      pure wrapper

    EFunctionDef mty mss name args body -> do
      body' <- liftExpr globalScope (scope <> Set.fromList args) body
      pure (EFunctionDef mty mss name args body')

    EBind lhs rhs -> do
      lhs' <- go lhs
      rhs' <- liftExpr globalScope (scope <> patVarSet lhs) rhs
      pure (EBind lhs' rhs')

    ELet lhs rhs -> do
      lhs' <- go lhs
      rhs' <- liftExpr globalScope (scope <> patVarSet lhs) rhs
      pure (ELet lhs' rhs')

    EAndThen a b -> do
      a' <- go a
      b' <- liftExpr globalScope (scope <> blockBound a) b
      pure (EAndThen a' b')

    EBlock es -> EBlock <$> goBlock scope es

    ECaseOf e binders -> do
      e' <- go e
      binders' <- forM binders $ \(EBinder pat, body) -> do
        pat' <- go pat
        body' <- liftExpr globalScope (scope <> patVarSet pat) body
        pure (EBinder pat', body')
      pure (ECaseOf e' binders')

    EApp ann f args     -> EApp ann      <$> go f <*> traverse go args
    EUnary op e         -> EUnary op     <$> go e
    EBinary op a b      -> EBinary op    <$> go a <*> go b
    ETupleLiteral es    -> ETupleLiteral <$> traverse go es
    EArrayLiteral es    -> EArrayLiteral <$> traverse go es
    EListLiteral es     -> EListLiteral  <$> traverse go es
    EListCons es e      -> EListCons     <$> traverse go es <*> go e
    EMapLiteral kvs     -> EMapLiteral   <$> traverse (traverseSnd go) kvs
    EMapPattern kvs     -> EMapPattern   <$> traverse (traverseSnd go) kvs
    EMapUpdate e kvs    -> EMapUpdate    <$> go e <*> traverse (traverseSnd go) kvs
    ETryAnyAny a b      -> ETryAnyAny    <$> go a <*> go b
    ERawErlangSource fmt kvs -> ERawErlangSource fmt <$> traverse (traverseSnd go) kvs

    EVar {}            -> pure expr
    EAtomLiteral {}    -> pure expr
    ENumericLiteral {} -> pure expr
    EStringLiteral {}  -> pure expr
    ECharLiteral {}    -> pure expr
    EFunRef {}         -> pure expr
    EComment {}        -> pure expr
    EAttribute {}      -> pure expr
    ESpec {}           -> pure expr
    EType {}           -> pure expr

  traverseSnd f (k, v) = (k,) <$> f v

  goBlock _ [] = pure []
  goBlock sc (e:es) = do
    e' <- liftExpr globalScope sc e
    es' <- goBlock (sc <> blockBound e) es
    pure (e' : es')

  blockBound (EBind lhs _) = patVarSet lhs
  blockBound (ELet lhs _)  = patVarSet lhs
  blockBound _             = Set.empty


-- ---------------------------------------------------------------------------
-- Free-variable analysis for a lambda's own clauses.
-- Bound = only the lambda's own EFunBinder patterns.
-- Does NOT subtract the enclosing scope — caller does that via globalScope.

freeVarsOfFun :: [(EFunBinder, Erl)] -> Set T.Text
freeVarsOfFun clauses =
  foldMap (\(EFunBinder pats, body) ->
    let ownBound = Set.fromList (concatMap patVars pats)
    in freeVars ownBound body
  ) clauses

freeVars :: Set T.Text -> Erl -> Set T.Text
freeVars bound = go
  where
  go (EVar v)
    | Set.member v bound = Set.empty
    | otherwise          = Set.singleton v

  go (EFunFull _ clauses) =
    foldMap (\(EFunBinder pats, body) ->
      let clauseBound = bound <> Set.fromList (concatMap patVars pats)
      in freeVars clauseBound body
    ) clauses

  go (EFunctionDef _ _ _ args body) =
    freeVars (bound <> Set.fromList args) body

  go (EBind lhs rhs) =
    freeVars (bound <> patVarSet lhs) rhs

  go (ELet lhs rhs) =
    freeVars (bound <> patVarSet lhs) rhs

  go (EAndThen a b) =
    go a <> freeVars (bound <> blockBound a) b

  go (EBlock es) = goBlock bound es
    where
    goBlock _ []        = Set.empty
    goBlock sc (e:rest) = freeVars sc e <> goBlock (sc <> blockBound e) rest

  go (ECaseOf e binders) =
    go e <> foldMap (\(EBinder pat, body) ->
      freeVars (bound <> patVarSet pat) body) binders

  go (EApp _ f args)   = go f <> foldMap go args
  go (EUnary _ e)      = go e
  go (EBinary _ a b)   = go a <> go b
  go (ETupleLiteral es)   = foldMap go es
  go (EArrayLiteral es)   = foldMap go es
  go (EListLiteral es)    = foldMap go es
  go (EListCons es e)     = foldMap go es <> go e
  go (EMapLiteral kvs)    = foldMap (go . snd) kvs
  go (EMapPattern kvs)    = foldMap (go . snd) kvs
  go (EMapUpdate e kvs)   = go e <> foldMap (go . snd) kvs
  go (ETryAnyAny a b)     = go a <> go b
  go (ERawErlangSource _ kvs) = foldMap (go . snd) kvs
  go _                    = Set.empty

  blockBound (EBind lhs _) = patVarSet lhs
  blockBound (ELet lhs _)  = patVarSet lhs
  blockBound _             = Set.empty


-- ---------------------------------------------------------------------------
-- Helpers

patVars :: Erl -> [T.Text]
patVars (EVar v)            = [v]
patVars (EBind lhs _)       = patVars lhs
patVars (ETupleLiteral ps)  = concatMap patVars ps
patVars (EListLiteral ps)   = concatMap patVars ps
patVars (EListCons ps tl)   = concatMap patVars ps ++ patVars tl
patVars (EMapPattern kvs)   = concatMap (patVars . snd) kvs
patVars _                   = []

patVarSet :: Erl -> Set T.Text
patVarSet = Set.fromList . patVars

prependFreeVarPats :: [T.Text] -> (EFunBinder, Erl) -> (EFunBinder, Erl)
prependFreeVarPats fvs (EFunBinder pats, body) =
  (EFunBinder (map EVar fvs ++ pats), body)

funArity :: [(EFunBinder, Erl)] -> Int
funArity []                         = 0
funArity ((EFunBinder pats, _) : _) = length pats

firstClauseBoundVars :: [(EFunBinder, Erl)] -> [T.Text]
firstClauseBoundVars []                         = []
firstClauseBoundVars ((EFunBinder pats, _) : _) = concatMap patVars pats
