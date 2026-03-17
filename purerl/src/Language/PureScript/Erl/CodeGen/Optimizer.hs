-- |
-- This module optimizes code in the simplified-Erlang intermediate representation.
--
-- The following optimizations are supported:
--
--  * Inlining of (>>=) and ret for the Eff monad
--
module Language.PureScript.Erl.CodeGen.Optimizer (optimize) where

import Prelude.Compat

import Control.Monad.Supply.Class (MonadSupply)

import Language.PureScript.Erl.CodeGen.AST
    ( everywhereOnErl, Erl(..), pattern EApp, Atom )
import Language.PureScript.Erl.CodeGen.Optimizer.MagicDo
    ( magicDo )
import Language.PureScript.Erl.CodeGen.Optimizer.Blocks
    ( collapseNestedBlocks )
import Language.PureScript.Erl.CodeGen.Optimizer.Common
    ( applyAll, applyAllM )
import Language.PureScript.Erl.CodeGen.Optimizer.Inliner
    ( beginBinds,
      etaConvert,
      evaluateIifes,
      inlineCommonOperators,
      inlineCommonValuesTopDown,
      inlineCommonValuesBottomUp,
      specialize,
      singleBegin, collectLists, replaceAppliedFunRefs, inlineCommonFnsM )
-- import Language.PureScript.Erl.CodeGen.Optimizer.Guards
--     ( inlineSimpleGuards )

import qualified Language.PureScript.Erl.CodeGen.Constants as EC
import Language.PureScript.Erl.CodeGen.Optimizer.Unused (removeUnusedFuns)
import Data.Map (Map)
import Language.PureScript.Erl.CodeGen.Optimizer.Memoize (addMemoizeAnnotations)
import Control.Monad ((<=<))
import Language.PureScript.Erl.CodeGen.Inliner qualified as Inliner
import Language.PureScript.Erl.CodeGen.InlineLocal qualified as InlineLocal
import Debug.Trace
import Data.Function ((&))

-- |
-- Apply a series of optimizer passes to simplified Javascript code
--
optimize :: MonadSupply m => [(Atom, Int)] -> [Erl] -> m [Erl]
-- optimize exports es = pure es
-- optimize exports es = pure (Inliner.inline es)
-- optimize exports es = removeUnusedFuns exports <$> pure (Inliner.inline es)
optimize exports es = do -- removeUnusedFuns exports <$> do
  -- es2 <-
  --     pure es
  -- let es3 = Inliner.inline es2
  -- es4 <- untilFixedPoint (traverse go) es2
  -- es4 <- untilFixedPoint (traverse go) es3
  -- let es5 = InlineLocal.inlineVarBinds es4
  -- es6 <- untilFixedPoint (traverse go) es5
  -- let es7 = InlineLocal.inlineVarBinds es6
  -- es8 <- untilFixedPoint (traverse go) es7
  es2 <- es
    -- & map (inlineCommonOperators EC.effect EC.effectDictionaries expander)
    -- & map (go)
    & map
      (\b ->
        b
          & inlineCommonOperators EC.effect EC.effectDictionaries id
      )
    & pure
  es2
    & mapM specialize
    & fmap (map (untilFix go))
    -- & Inliner.inline
    -- & map (untilFix go)
    -- & Inliner.inline
    -- & map (untilFix go)
    -- & Inliner.inline
    -- & map (untilFix go)
    -- & map addMemoizeAnnotations
  -- pure $ es4

  where
  go erl =
    erl
      & inlineCommonValuesTopDown id -- expander
      & inlineCommonValuesBottomUp id -- expander
      -- Compilation took 107841 ms -- only topdown
      -- Compilation took 104441 ms -- only topdown
      -- Compilation took 102926 ms -- only bottomup
      -- Compilation took 102995 ms -- only bottomup
      -- Compilation took 109079 ms -- both


--   do
--    erl' <-
--      -- INVARIANT[drathier]: these transforms must never duplicate expressions, or they might duplicate bound variables without renaming the copies. We could (and probably should) rewrite them to actually inline variables, but we could also implement that step later, which we have already done in the inliner module.
--        erl
--        & pure
--
--    -- erl2 <- Inliner.inline erl
--
--    -- erl'' <- untilFixedPoint tidyUp
--    --   =<< untilFixedPoint (return . magicDo expander)
--    --   erl'
--    -- pure $ addMemoizeAnnotations erl''
--    pure $ erl'
--    -- pure $ addMemoizeAnnotations erl2

  -- expander = id -- buildExpander es

  tidyUp :: MonadSupply m => Erl -> m Erl
  tidyUp = applyAllM
    [ pure . collapseNestedBlocks
    -- , pure . inlineSimpleGuards
    , pure . beginBinds
    , pure . evaluateIifes -- NOTE[drathier]: skipping this step doesn't change the resulting output/ folder contents at all; presumably it's handled by the etaConvert step
    , pure . singleBegin
    , pure . replaceAppliedFunRefs
    , pure . collectLists
    , etaConvert -- NOTE[drathier]: this removes/inlines IIFE's statefully, but `evaluateIifes` step before also perhaps does the same?
    ]


untilFixedPoint :: Show a => (Monad m, Eq a) => (a -> m a) -> a -> m a
untilFixedPoint f = go 10
  where
  go 0 a = pure a -- trace (show ("untilFixedPoint bailed out", a)) $ pure a
  go n a = do
   a' <- f a
   if a' == a then return a' else go (n-1) a'

untilFix :: Show a => Eq a => (a -> a) -> a -> a
untilFix f = go 10
  where
  go 0 a = a -- trace (show ("untilFix bailed out", a)) $ a
  go n a =
   let a2 = f a in
   if a2 == a then a2 else go (n-1) a2


-- |
-- Take all top-level ASTs and return a function for expanding top-level
-- variables during the various inlining steps in `optimize`.
--
-- Everything that gets inlined as an optimization is of a form that would
-- have been lifted to a top-level binding during CSE, so for purposes of
-- inlining we can save some time by only expanding variables bound at that
-- level and not worrying about any inner scopes.
--
buildExpander :: [Erl] -> Erl -> Erl
buildExpander = replaceAtoms . foldr go []
  where
  go = \case
    EFunctionDef _ _ name [] e | isSimpleApp e  -> ((name, e) :)
    _ -> id

  replaceAtoms updates = everywhereOnErl (replaceAtom updates)

  replaceAtom updates = \case
    EApp _ (EAtomLiteral a) [] | Just e <- lookup a updates
      -> replaceAtoms updates e
    other -> other

  -- simple nested applications that look similar to floated synthetic apps
  isSimpleApp (EApp _ e1 es) = isSimpleApp e1 && all isSimpleApp es
  isSimpleApp (EAtomLiteral _) = True
  isSimpleApp _ = False
