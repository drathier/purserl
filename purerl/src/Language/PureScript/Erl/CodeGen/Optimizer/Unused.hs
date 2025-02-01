-- | Removes unused variables
module Language.PureScript.Erl.CodeGen.Optimizer.Unused
  ( removeUnusedFuns,
  )
where

import Control.Monad (filterM)
import Data.Monoid (Any (..))
import qualified Data.Set as S
import Data.Text (Text)
import Language.PureScript.Erl.CodeGen.AST
  ( Atom (..),
    Erl (..),
    pattern EApp,
    everything,
  )
import Prelude.Compat
import Protolude (mapMaybe)

-- TODO not recognising external self-module calls, which should not be generated right now, who are we anyway

-- TODO not recognising fns only used in top-lvl floated synthetic apps reachable from other code

removeUnusedFuns :: [(Atom, Int)] -> [Erl] -> [Erl]
removeUnusedFuns exps = error "drathier: removeUnusedFuns doesn't play nice with inlining, apparently. It can remove referenced functions, e.g. `fun myTopLevelFn/2` isn't recognized as a top-level-same-module ref, even though it's in the code below. Not sure what happened. Nvm, `everything` didn't recurse into ELet. Nope, that still didn't fix it."
{-
It somehow doesn't recognize `EFunRef (Atom Nothing "unsafeCoerce") 1` as a usage/reference to unsafeCoerce/1 in the below example

output/Data.Symbol/data_symbol@ps.erl
```
reifySymbol(Data@_Symbol_reifySymbol_s_0,Data@_Symbol_reifySymbol_f_1) ->
  Data@_Symbol_reifySymbol_coerce_2 = fun unsafeCoerce/1,
  (((Data@_Symbol_reifySymbol_coerce_2(Data@_Symbol_reifySymbol_f_1))(#{reflectSymbol=>fun (Data@_Symbol_reifySymbol_@dollar0_4) ->
    Data@_Symbol_reifySymbol_s_0
  end}))({ proxy }))
.
```
opt:
```
EFunctionDef (Just (TFun [TAlias (Atom Nothing "binary") [],TFun [TAny] (TFun [TAny] TAny)] TAny)) (Just ss) (Atom Nothing "reifySymbol") ["Data@_Symbol_reifySymbol_s_0","Data@_Symbol_reifySymbol_f_1"] (ELet (EBind (EVar "Data@_Symbol_reifySymbol_coerce_2") (EFunRef (Atom Nothing "unsafeCoerce") 1)) (EApp RegularApp (EApp RegularApp (EApp RegularApp (EVar "Data@_Symbol_reifySymbol_coerce_2") [EVar "Data@_Symbol_reifySymbol_f_1"]) [EMapLiteral [(AtomPS Nothing "reflectSymbol",EFunFull Nothing [(EFunBinder [EVar "Data@_Symbol_reifySymbol_@dollar0_4"] Nothing,EVar "Data@_Symbol_reifySymbol_s_0")])]]) [ETupleLiteral [EAtomLiteral (Atom Nothing "proxy")]])),

```
non-opt:
```
EFunctionDef (Just (TFun [TAlias (Atom Nothing "binary") [],TFun [TAny] (TFun [TAny] TAny)] TAny)) (Just ss) (Atom Nothing "reifySymbol") ["_@10","_@11"] (EApp RegularApp (EApp RegularApp (EFunFull Nothing [(EFunBinder [EVar "Data@_Symbol_reifySymbol_s_0"] Nothing,EFunFull Nothing [(EFunBinder [EVar "Data@_Symbol_reifySymbol_f_1"] Nothing,ELet (EBind (EVar "Data@_Symbol_reifySymbol_coerce_2") (EFunRef (Atom Nothing "unsafeCoerce") 1)) (EApp RegularApp (EApp RegularApp (EApp RegularApp (EVar "Data@_Symbol_reifySymbol_coerce_2") [EFunFull Nothing [(EFunBinder [EVar "Data@_Symbol_reifySymbol_@dollardictIsSymbol1_3"] Nothing,EApp RegularApp (EVar "Data@_Symbol_reifySymbol_f_1") [EVar "Data@_Symbol_reifySymbol_@dollardictIsSymbol1_3"])]]) [EMapLiteral [(AtomPS Nothing "reflectSymbol",EFunFull Nothing [(EFunBinder [EVar "Data@_Symbol_reifySymbol_@dollar0_4"] Nothing,EVar "Data@_Symbol_reifySymbol_s_0")])]]) [ETupleLiteral [EAtomLiteral (Atom Nothing "proxy")]]))])]) [EVar "_@10"]) [EVar "_@11"]),
......
EFunctionDef Nothing Nothing (Atom Nothing "unsafeCoerce") [] (ELet (EBind (EVar "F27") (EFunFull Nothing [(EFunBinder [EVar "_@26"] Nothing,EApp RegularApp (EAtomLiteral (Atom (Just "data_symbol@foreign") "unsafeCoerce")) [EVar "_@26"])])) (EVar "F27")),
EFunctionDef Nothing Nothing (Atom Nothing "unsafeCoerce") ["_@29"] (EApp RegularApp (EApp RegularApp (EAtomLiteral (Atom Nothing "unsafeCoerce")) []) [EVar "_@29"])]
```
-}
removeUnusedFuns exps = loop
  where
    expsSet =
      S.fromList $
        mapMaybe
          ( \case
              (Atom _ name, n) -> Just (name, n)
              _ -> Nothing
          )
          exps

    loop :: [Erl] -> [Erl]
    loop asts = if changed then loop asts' else asts
      where
        used =
          expsSet
            <> foldMap
              ( everything
                  (<>)
                  ( \case
                      EFunRef (Atom Nothing name) n -> S.singleton (name, n)
                      EApp _ (EAtomLiteral (Atom Nothing name)) args -> S.singleton (name, length args)
                      _ -> S.empty
                  )
              )
              asts
        (Any changed, asts') = filterM (anyFalses . isInUsedSet used) asts

    isInUsedSet :: S.Set (Text, Int) -> Erl -> Bool
    isInUsedSet used = \case
      EFunctionDef _ _ (Atom Nothing name) vars _e ->
        (name, length vars) `S.member` used
      _ -> True

    anyFalses :: Bool -> (Any, Bool)
    anyFalses x = (Any (not x), x)
