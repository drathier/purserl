{-# LANGUAGE ScopedTypeVariables #-}

module Language.PureScript.Erl.CodeGen.EffectLift (effectLift) where

import Prelude.Compat

import qualified Data.Text as T

import Language.PureScript.Erl.CodeGen.AST


effectLift :: [Erl] -> [Erl]
effectLift decls =
  concatMap genTwin (map rewriteCallSites decls)


-- | If a top-level function's body is a zero-arity thunk, emit a $e twin
-- with the thunk body unwrapped, and rewrite the original to delegate to it.
genTwin :: Erl -> [Erl]
genTwin decl@(EFunctionDef mty mss name@(Atom mq baseName) args (EFun0 _ thunkBody)) =
  let twinName = Atom mq (baseName <> "$e")
      twin     = EFunctionDef mty mss twinName args thunkBody
      wrapper  = case args of
        [] -> EFunRef twinName 0
        _  -> EFun0 Nothing (EApp RegularApp (EAtomLiteral twinName) (map EVar args))
      original = EFunctionDef mty mss name args wrapper
  in [twin, original]
genTwin other = [other]


-- | Rewrite (f(args))() -> f$e(args) everywhere in a declaration.
rewriteCallSites :: Erl -> Erl
rewriteCallSites = everywhereOnErl go
  where
  go (EApp _ (EApp ann (EAtomLiteral (Atom mq name)) args) []) =
    EApp ann (EAtomLiteral (Atom mq (name <> "$e"))) args
  go (EApp _ (EFunRef (Atom mq name) 0) []) =
    EApp RegularApp (EAtomLiteral (Atom mq (name <> "$e"))) []
  go other = other
