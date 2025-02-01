module Language.PureScript.Erl.CodeGen.Optimizer.Memoize
  ( addMemoizeAnnotations,
  )
where

import Prelude

import Language.PureScript.Erl.CodeGen.AST
    ( Erl(..), Atom, everywhereOnErl, pattern EApp, AppAnnotation (..), pattern EFun0, litAtom, qualFunCall, everything, EFunBinder(..) )
import Data.Map as Map
import Language.PureScript.Erl.Pretty ( prettyPrintErl )
import Data.Hashable (hash)
import Data.Text qualified as T
import Data.List (nub, concatMap)

addMemoizeAnnotations :: Erl -> Erl
addMemoizeAnnotations = everywhereOnErl go
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
-- memoizeAnnotation emem = emem
memoizeAnnotation emem =
  let
      -- uniqueVarPrefix = "X_UniqueVar_"
      collectDep expr =
        case expr of
          -- EVar var | not (uniqueVarPrefix `T.isPrefixOf` var) -> [var]
          EVar var -> [var]
          _ -> []
      externalVariables =
        nub $
        everything
          (<>)
          collectDep
          -- (\erl ->
          --   case erl of
          --     EApp _ fn args ->
          --       collectDep fn <> concatMap collectDep args
          --     _ -> []
          -- )
          emem
  in
  case externalVariables of
    (_:_) -> emem
    [] ->
      let
          extVarsAsAtoms = Prelude.map litAtom externalVariables
          extVarsAsErl = Prelude.map EVar externalVariables
          -- key = T.replace "\n" "" $ T.replace " " "" $ prettyPrintErl id [emem]
          ememAstHash = (abs (hash (prettyPrintErl id [emem])))
          key = T.pack (show ememAstHash)
          keyAtom = litAtom (key)
          -- uniqueVar = uniqueVarPrefix <> key
          -- NOTE[drathier]: ?MODULE is needed in key because common names like `append` can be duplicated in many modules, sometimes with different meaning, even if the local ast is identical. It causes some keys to be duplicated, but that's the trade-off for now.
          -- keyTuple = ETupleLiteral ([litAtom "PsMemoKey", keyAtom, EVar "?MODULE"] <> extVarsAsErl)
          keyTuple = ETupleLiteral ([litAtom "PsMemoKey", EVar "?LINE", EVar "?MODULE"])
      in
        (qualFunCall "Elixir.Zen.TermCache" "cache" [keyTuple, EFun0 Nothing emem])
--        ETryAnyAny
--          -- (qualFunCall "persistent_term" "get" [keyTuple])
--          (qualFunCall "Elixir.Zen.TermCache" "get" [keyTuple])
--          -- (EBlock
--          --   [ EVarBind uniqueVar (qualFunCall "Elixir.Zen.TermCache" "get" [keyTuple])
--          --   , EVarBind uniqueVar emem
--          --   ]
--          -- )
--          (qualFunCall "Elixir.Zen.TermCache" "put" [keyTuple, emem])
--          -- (EBlock
--          --   ( -- [ (qualFunCall "erlang" "display" [ETupleLiteral ([EVar "?MODULE", EVar "?LINE", EVar "?FUNCTION_NAME", keyAtom, litAtom "ExtVars"] <> extVarsAsAtoms)]) ] <>
--          --   [ EVarBind uniqueVar emem
--          --   , qualFunCall "Elixir.Zen.TermCache" "put" [keyTuple, EVar uniqueVar]
--          --   , EVar uniqueVar
--          --   ]
--          --   )
--          -- )
