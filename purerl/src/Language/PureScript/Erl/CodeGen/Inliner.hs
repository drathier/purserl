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
    ( everywhereOnErl, Erl(..), pattern EApp, Atom(..), everywhereOnErlTopDownM, everything )
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
import Control.Monad.State (MonadState(..), State(..), gets, modify, runState)
import Debug.Trace (traceM)


data DB = DB
  { inlineableLocal :: Map (Atom, Int) Erl
  }

emptyDB = DB mempty

inline :: [Erl] -> [Erl]
inline erls =
  let
      initialDB = DB $ Map.empty

      runOneDef :: Erl -> State DB Erl
      runOneDef e = do
        -- traceM (show ("runOneDef", e))
        db <- get
        case e of
          EApp _ (EAtomLiteral atom) args | Just (EFunctionDef _ _ _ fargvars fbody) <- Map.lookup (atom, length args) (inlineableLocal db) -> do
            traceM (show ("runOneDef-inlining", (atom, length args)))
            pure (replaceIdents (zip fargvars args) fbody)
          -- EApp _ (EAtomLiteral atom) args | Just v <- Map.lookup (atom, length args) (inlineableLocal db) -> pure v
          _ -> pure e

      run :: [Erl] -> State DB [Erl]
      run [] = pure []
      run (def@(EFunctionDef _ _ name fargvars fbody):defs) = do
        let nameString = runAtom name
        -- traceM (show ("run", name, length fargvars))
        resDef <- everywhereOnErlTopDownM runOneDef def

        let mentionsSelf =
              everything (||)
                (\erl ->
                  case erl of
                    -- [drathier]: just matching the atom also finds record keys, which is too strict. Matching just direct function calls perhaps isn't strict enough, `A = a, A()`
                    -- EAtomLiteral atom ->
                    EApp _ (EAtomLiteral atom) _ ->
                      let atomString = runAtom atom
                      in atomString == nameString
                    _ -> False
                ) resDef
        case mentionsSelf of
          False -> do
            db <- get
            put (db {inlineableLocal = Map.insert (name, length fargvars) def (inlineableLocal db)})
          True -> pure ()
        (resDef:) <$> run defs
      run (def:defs) = do
        traceM (show ("top-level not FunctionDef", def))
        (def:) <$> run defs

  in
  fst $ runState (run erls) initialDB
