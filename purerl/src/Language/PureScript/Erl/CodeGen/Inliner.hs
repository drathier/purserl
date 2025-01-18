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
    ( everywhereOnErl, Erl(..), EFunBinder(..), EBinder(..), Guard(..), pattern EApp, Atom(..), everywhereOnErlTopDownM, everywhereOnErlBottomUpM, everything )
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
inline erls =
  let
      initialDB = DB Map.empty 0

      runOneDef :: Erl -> [Text] -> Erl -> State DB Erl
      runOneDef whereami fargvars e = do
        --traceM (show ("runOneDef", e))
        db <- get
        case e of
          EApp _ (EAtomLiteral atom) args | Just (EFunctionDef _ _ _ fargvars fbody) <- Map.lookup (atom, length args) (inlineableLocal db) -> do
            -- pure (replaceIdents (zip fargvars args) fbody)
            suffix <- freshInlineSuffix
            -- traceM (show ("runOneDef-inlining", (atom, length args, suffix, whereami)))
            pure (replaceOrSuffixIdents suffix (zip fargvars args) fbody)
          -- EApp _ (EAtomLiteral atom) args | Just v <- Map.lookup (atom, length args) (inlineableLocal db) -> pure v
          _ -> pure e

      run :: [Erl] -> State DB [Erl]
      run [] = pure []
      run (def@(EFunctionDef _ _ name fargvars fbody):defs) = do
        let nameString = runAtom name
        db <- get
        -- traceM (show ("run", name, length fargvars, Map.keys (inlineableLocal db)))
        resDef <- everywhereOnErlTopDownM (runOneDef def fargvars) def

        let mentionsSelf =
              everything (||)
                (\erl ->
                  case erl of
                    -- [drathier]: just matching the atom also finds record keys, which is too strict. Matching just direct function calls perhaps isn't strict enough, `A = a, A()`
                    -- EAtomLiteral atom ->
                    EApp _ (EAtomLiteral atom) _ ->
                      let atomString = runAtom atom
                      in atomString == nameString
                    EFunRef atom _ ->
                      let atomString = runAtom atom
                      in atomString == nameString
                    _ -> False
                ) resDef
        case mentionsSelf || elem name forbiddenInlineFunctions of
          False -> do
            db <- get
            put (db {inlineableLocal = Map.insert (name, length fargvars) def (inlineableLocal db)})
          True -> pure ()
        (resDef:) <$> run defs
      run (def:defs) = do
        -- traceM (show ("top-level not FunctionDef", def))
        (def:) <$> run defs

  in
  -- trace (show ("erls", [name, EFunctionDef _ _ name fargvars])) $
  fst $ runState (run erls) initialDB

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
