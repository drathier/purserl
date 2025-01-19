-- |
-- This module optimizes code in the simplified-Erlang intermediate representation.
--
-- The following optimizations are supported:
--
--  * Inlining of (>>=) and ret for the Eff monad
--
module Language.PureScript.Erl.CodeGen.Inliner (inline) where

import Prelude.Compat

import Control.Monad.Supply.Class (MonadSupply)

import Language.PureScript.Erl.CodeGen.Common (runAtom)
import Language.PureScript.Erl.CodeGen.AST
    ( everywhereOnErl, Erl(..), AppAnnotation(..), EFunBinder(..), EBinder(..), Guard(..), pattern EApp, Atom(..), everywhereOnErlTopDownM, everywhereOnErlBottomUpM, everything )
import Language.PureScript.Erl.CodeGen.Optimizer.MagicDo
    ( magicDo )
import Language.PureScript.Erl.CodeGen.Optimizer.Blocks
    ( collapseNestedBlocks )
import Language.PureScript.Erl.CodeGen.Optimizer.Common
    ( applyAll, applyAllM, replaceIdents )
import Language.PureScript.Erl.CodeGen.Optimizer.Inliner
    ( beginBinds,
      etaConvert,
      evaluateIifes,
      inlineCommonOperators,
      inlineCommonValuesTopDown,
      inlineCommonValuesBottomUp,
      singleBegin, collectLists, replaceAppliedFunRefs, inlineCommonFnsM )
import Language.PureScript.Erl.CodeGen.Optimizer.Guards
    ( inlineSimpleGuards )

import qualified Language.PureScript.Erl.CodeGen.Constants as EC
import Language.PureScript.Erl.CodeGen.Optimizer.Unused (removeUnusedFuns)
import Data.Map (Map)
import Data.Map qualified as Map
import Language.PureScript.Erl.CodeGen.Optimizer.Memoize (addMemoizeAnnotations)
import Control.Monad ((<=<))
import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad.State (MonadState(..), State(..), gets, modify, runState)
import Debug.Trace (traceM, trace)
import Data.List qualified as List
import Data.Graph qualified as G
import Language.PureScript.PSString qualified as PSString
import Data.Maybe (fromJust)

forbiddenInlineFunctions = Atom Nothing <$> ["@runtime_lazy"]

data DB = DB
  { inlineableLocal :: Map (Atom, Int) Erl
  , inlineCounter :: Int
  }

freshInlineSuffix = do
  db <- get
  let ret = inlineCounter db
  put (db {inlineCounter = inlineCounter db + 1})
  pure (T.pack ("_in" <> show ret))

inline :: [Erl] -> [Erl]
inline unsortedErls =
  let
      (unsafeToInline, erls) = topoSort unsortedErls

      initialDB = DB Map.empty 0

      runOneDef :: Erl -> [Text] -> Erl -> State DB Erl
      runOneDef whereami fargvars e = do
        -- traceM (show ("runOneDef", e))
        db <- get
        case e of

          EApp _ (EAtomLiteral atom) args | Just (EFunctionDef _ _ _ fargvars fbody) <- Map.lookup (atom, length args) (inlineableLocal db), inlineCounter db > 30 -> trace (show ("drathier: inlineCounter too large, would've inlined but bailing out", "asdf", e)) $ pure e
          EApp _ (EAtomLiteral atom) args | Just (EFunctionDef _ _ _ fargvars fbody) <- Map.lookup (atom, length args) (inlineableLocal db) -> do
            -- pure (replaceIdents (zip fargvars args) fbody)
            suffix <- freshInlineSuffix
            traceM (show ("runOneDef-inlining", (atom, length args, suffix)))
            pure (replaceOrSuffixIdents suffix (zip fargvars args) fbody)
          -- EApp _ (EAtomLiteral atom) args | Just v <- Map.lookup (atom, length args) (inlineableLocal db) -> pure v
          _ -> pure e

      run :: [Erl] -> State DB [Erl]
      run [] = pure []
      run (def@(EFunctionDef _ _ name fargvars fbody):defs) = do
        let nameString = runAtom name
        let arity = length fargvars
        db <- get
        -- traceM (show ("run", name, arity, Map.keys (inlineableLocal db)))
        !_ <- case Map.lookup (name, arity) (inlineableLocal db) of
          Nothing -> pure ()
          Just _ -> error (show ("run called for already inlineable local", (name, arity), def))

        resDef <- everywhereOnErlTopDownM (runOneDef def fargvars) def

        let mentionsSelf =
              everything (||)
                (\erl ->
                  case erl of
                    -- [drathier]: just matching the atom also finds record keys, which is too strict. Matching just direct function calls perhaps isn't strict enough, `A = a, A()`
                    -- EAtomLiteral atom ->
                    EApp _ (EAtomLiteral atom) _ ->
                      runAtom atom == nameString
                    EFunRef atom _ ->
                      runAtom atom == nameString
                    _ -> False
                ) resDef
        case mentionsSelf || elem name forbiddenInlineFunctions || elem (runAtom name, arity) unsafeToInline of
          True -> pure ()
          False -> do
            db <- get
            put (db {inlineableLocal = Map.insertWith (\a b -> error (show ("Inliner insertWith", a, b))) (name, arity) def (inlineableLocal db)})
        (resDef:) <$> run defs
      run (def:defs) = do
        -- traceM (show ("top-level not FunctionDef", def))
        (def:) <$> run defs

  in
  -- trace (show ("erls", [(name, length fargvars) | EFunctionDef _ _ name fargvars _ <- erls])) $
  fst $ runState (run erls) initialDB

mentionedTopLevelValues :: Erl -> [(T.Text, Int)]
mentionedTopLevelValues expr =
  everything (<>)
    (\erl ->
      case erl of
        -- EApp RegularApp (EApp RegularApp (EApp RegularApp almostRunFn3 []) [EApp RegularApp almostRuntimeLazy args1]) args2 ->
        --   trace (show ("NEEDLE_runtime_lazy2", almostRunFn3, almostRuntimeLazy, args1, args2)) $ []
        -- EApp RegularApp (EApp RegularApp (EApp RegularApp almostRunFn3 []) [EApp RegularApp almostRuntimeLazy args1]) args2 ->
        --   trace (show ("NEEDLE_runtime_lazy2", almostRunFn3, almostRuntimeLazy, args1, args2)) $ []
        -- [drathier]: just matching the atom also finds record keys, which is too strict. Matching just direct function calls perhaps isn't strict enough, `A = a, A()`
        -- EAtomLiteral atom ->
        -- EAtomLiteral atom -> [(runAtom atom, 42)]
        -- EStringLiteral str -> [(fromJust (PSString.decodeString str), 333)]
        EApp _ (EAtomLiteral atom) args -> [(runAtom atom, length args)]
        EFunRef atom arity -> [(runAtom atom, arity)]
        EApp RegularApp (EApp RegularApp (EApp RegularApp (EAtomLiteral (Atom (Just "data_function_uncurried@ps") "runFn3")) []) [EApp RegularApp (EAtomLiteral (Atom Nothing "@runtime_lazy")) [EVar _]]) [EStringLiteral fnNameAsString] -> [(fromJust (PSString.decodeString fnNameAsString), 0)]
        _ -> []
    )
    expr

topoSort erls =
  let
      f erl =
        case erl of
          EFunctionDef _ _ name fargvars fbody -> (erl, (runAtom name, length fargvars), mentionedTopLevelValues fbody)
          _ -> error (show ("topoSort-drathier-not-EFunctionDef", erl))

      edges = map f erls
      (graph, vertexToNode, keyToVertex) = G.graphFromEdges edges

      sccMap sccVertex =
        case sccVertex of
          G.AcyclicSCC a -> G.AcyclicSCC (vertexToNode a)
          G.CyclicSCC ax -> G.CyclicSCC (map vertexToNode ax)

      topo = G.stronglyConnCompR edges
      topoErls = concatMap (\case
        (G.AcyclicSCC (e,_,_)) -> [e]
        (G.CyclicSCC es) -> map (\(e,_,_) -> e) es
        ) topo

      -- FIXME[drathier]: this is overly pessimistic. We could look at the SCC without a single fundef, forall fundefs, to see what can be inlined into each of the fundefs. Each fundef will prohibit inlining itself. For example, odd/even mutrec functions can each be inlined into eachother, leaving two singly recursive functions afterwards. Looking at the SCC, [odd,even] without [even] is acyclic, and safe to inline.
      unsafeToInline = map (\(_,k,_) -> k) $ concat $ [v | G.CyclicSCC v <- topo]
  in
  -- (\res -> trace (show ("drathier-graph0", edges)) res) $
  -- (\res -> trace (show ("drathier-graph1", map (\(_,k,o) -> (k,o)) edges)) res) $
  -- (\res -> trace (show ("drathier-graph2", map (fmap (\(_,(katom, karity),out) -> (katom, karity, out))) topo)) res) $
  -- (\res -> trace (show ("drathier-graph3", unsafeToInline)) res) $
  (unsafeToInline, topoErls)

replaceOrSuffixIdents :: Text -> [(Text, Erl)] -> Erl -> Erl
replaceOrSuffixIdents suffix replacements erl =
  -- trace (show ("replaceOrSuffixIdents", suffix, replacements, "erl", erl)) $
  let
    rec e =
      -- trace (show ("rec", e)) $
      replaceOrSuffixIdents suffix replacements e
    -- recWithout fields e = replaceOrSuffixIdents suffix (filter (\v -> elem fields v == False) replacements) e

    replace v =
      -- trace (show ("replace", v)) $
      case v of
        "_" -> EVar v
        _ ->
          case List.lookup v replacements of
            Nothing -> EVar (onVar v)
            Just replacement -> replacement

    onVar v =
      -- trace (show ("onVar", v)) $
      case v of
        "_" -> v
        _ -> v <> suffix

    onBinder (EFunBinder erl mguard, rhs) =
      -- trace (show ("onBinder", erl, mguard, rhs)) $
      -- we also have to recurse into funbinders explicitly here, not just apply one level of rewrites
      (EFunBinder (map rec erl) (onGuard <$> mguard), rhs)

    onCaseBinder (b,c) =
      -- trace (show ("onCaseBinder", b, c)) $
      (case b of
        EBinder rhs -> EBinder (rec rhs)
        EGuardedBinder rhs guard -> EGuardedBinder (rec rhs) (onGuard guard)
      , c
      )

    onGuard (Guard erl) =
      -- trace (show ("onGuard", Guard erl)) $
      -- guards are not recursed into either
      Guard (rec erl)

    rewrite :: Erl -> Erl
    rewrite e =
      -- trace (show ("rewrite", e)) $
      case e of
        EFunctionDef mt mss atom args rhs ->
          EFunctionDef mt mss atom (map onVar args) rhs
        EFunFull mname binders ->
          -- binders are Erl, but everywhereOnErl doesn't handle the EFunBinder part for us, so we do it here ourselves, on only that part
          EFunFull (onVar <$> mname) (onBinder <$> binders)
        ECaseOf cond binders ->
          -- ECaseOf fst binders aren't recursed into, so we do it manually
          ECaseOf cond $ map onCaseBinder binders

        EVar v -> replace v
        other -> other
  in
  -- trace (show ("everywhereOnErl", erl)) $
  everywhereOnErl rewrite erl
