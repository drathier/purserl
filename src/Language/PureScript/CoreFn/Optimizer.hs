module Language.PureScript.CoreFn.Optimizer (optimizeCoreFn) where

import Protolude hiding (Type, moduleName)

import Control.Monad.Supply (Supply)
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.CSE (optimizeCommonSubexpressions)
import Language.PureScript.CoreFn.Expr (Bind, Expr(..))
import Language.PureScript.CoreFn.Module (Module(..))
import Language.PureScript.CoreFn.Traversals (everywhereOnValues)
import Language.PureScript.Constants.Libs qualified as C
import Debug.Trace qualified as Debug
-- |
-- CoreFn optimization pass.
--
optimizeCoreFn :: Module Ann -> Supply (Module Ann)
optimizeCoreFn m = fmap (\md -> m {moduleDecls = md}) . optimizeCommonSubexpressions (moduleName m) . optimizeModuleDecls $ moduleDecls m

optimizeModuleDecls :: [Bind Ann] -> [Bind Ann]
optimizeModuleDecls = map transformBinds
  where
  (transformBinds, _, _) = everywhereOnValues identity transformExprs identity
  transformExprs
    = optimizeDataFunctionApply

optimizeDataFunctionApply :: Expr a -> Expr a
optimizeDataFunctionApply e =
  case e of
    (App a (App _ (Var _ fn) x) y)
      | C.I_functionApply <- fn -> App a x y
      | C.I_functionApplyFlipped <- fn -> App a y x

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
    (App a (Var b fn) (Var c impl)) | C.I_bind <- fn, C.I_bindEffect <- impl -> Var a C.I_effectBindE
    -- (App a (Var b fn) (Var c impl)) | C.I_eBind <- fn, C.I_bindEffect <- impl -> Var a C.I_effectBindE



    -- (control_applicative@ps:pure((control_applicative@ps:applicativeArray())))
    (App a (Var b fn) (Var c impl)) | C.I_pure <- fn, C.I_applicativeArray <- impl -> Var a C.I_arrayPure
    (App a (Var b fn) (Var c impl)) | C.I_pure <- fn, C.I_applicativeMaybe <- impl -> Var a C.I_maybePure
    (App a (Var b fn) (Var c impl)) | C.I_pure <- fn, C.I_applicativeEither <- impl -> Var a C.I_eitherPure
    -- (control_applicative@ps:pure((effect@ps:applicativeEffect())))
    (App a (Var b fn) (Var c impl)) | C.I_pure <- fn, C.I_applicativeEffect <- impl -> Var a C.I_effectPureE

    -- ASSUMPTION[drathier]: all discard instances are implemented as `discard = bind`
    -- (control_bind@ps:discard((control_bind@ps:discardUnit()), (effect@ps:bindEffect())))
    (App a (App b (Var c fn) (Var d _discardImpl)) (Var e bindImpl)) | C.I_discard <- fn, C.I_bindEffect <- bindImpl -> Var a C.I_effectBindE
    -- (e@ps:discard((control_bind@ps:discardUnit()))) -- wrong discard
    (App a (Var b fn) (Var c _discardImpl)) | C.I_eDiscard <- fn -> Var a C.I_effectBindE
    -- (App a (Var b fn) (Var c impl)) | C.I_discard <- fn, C.I_eDiscard <- impl -> Var a C.I_effectBindE

    _ -> e
