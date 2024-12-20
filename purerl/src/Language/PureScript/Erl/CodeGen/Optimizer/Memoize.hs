module Language.PureScript.Erl.CodeGen.Optimizer.Memoize
  ( addMemoizeAnnotations,
  )
where

import Prelude

import Language.PureScript.Erl.CodeGen.AST
    ( Erl(..), Atom, everywhereOnErl, pattern EApp, AppAnnotation (..), pattern EFun0, litAtom, qualFunCall )
import Data.Map as Map
import Language.PureScript.Erl.Pretty ( prettyPrintErl )

addMemoizeAnnotations :: Map Atom Int -> Erl -> Erl
addMemoizeAnnotations _memoizable = everywhereOnErl go
  where
  go e = case e of
    EApp SyntheticApp _ _
      -> memoizeAnnotation e
    -- without using the annotation, but inferring things that seem to have fully applied tc args
    -- EApp' _ (EAtomLiteral f) args
    --   | Just n <- Map.lookup f memoizable
    --   , length args == n
    --   -> memoizeAnnotation e
    other -> other

memoizeAnnotation :: Erl -> Erl
-- memoizeAnnotation emem = EApp RegularApp (EVar "?MEMOIZE") [emem]
-- memoizeAnnotation emem = EApp RegularApp (EVar "'Elixir.PS.Util.Memoize':persistent_term") [EFun0 Nothing emem]
memoizeAnnotation emem =
  let
      key = litAtom (prettyPrintErl id [emem])
  in
  ETryAnyAny
    (qualFunCall "persistent_term" "get" [key])
    (EBlock
      [ EVarBind "X" emem
      , qualFunCall "persistent_term" "put" [key, EVar "X"]
      , EVar "X"
      ]
    )
