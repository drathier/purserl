-- |
-- This module optimizes code in the simplified-Erlang intermediate representation.
--
-- The following optimizations are supported:
--
--  * Inlining of (>>=) and ret for the Eff monad
--
module Language.PureScript.Erl.CodeGen.InlineLocal (inlineVarBinds) where

import Prelude.Compat

import Control.Monad.Supply.Class (MonadSupply)

import Language.PureScript.Erl.CodeGen.Common (runAtom)
import Language.PureScript.Erl.CodeGen.AST
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
import Data.UnionFind.ST qualified as UF
import Data.STRef as ST
import Control.Monad.ST as ST
import Control.Applicative ((<|>))

forbiddenInlineFunctions = Atom Nothing <$> ["@runtime_lazy"]

data DB = DB
  { _stack :: [STrack]
  }
  deriving (Show)


data STrack
  = SRead Text
  | SDef Text
  | SWrite Text Erl
  | SAlias Text Text
  deriving (Show)

track e t = do
  db <- get
  put (db {_stack = t : _stack db})
  pure ()

--freshInlineSuffix = do
--  db <- get
--  let ret = inlineCounter db
--  put (db {inlineCounter = inlineCounter db + 1})
--  pure (T.pack ("_iv" <> show ret))

initialDB = DB []

processStack stack =
  let cs = compressStack (reverse stack) in
  let actions = actionStack cs in
  -- trace (show ("processStack", ("cs", cs), ("actions", actions)))
  actions

compressStack stack =
  case stack of
    SAlias a b : SRead a1 : SRead b1 : rest | a == a1, b == b1 -> SAlias a b : compressStack rest
    SWrite a b : SRead a1 : rest | a == a1 -> SWrite a b : compressStack rest
    SDef a : rest -> SDef a : compressStack rest
    SRead a : rest -> SRead a : compressStack rest
    [] -> []
    other -> [SRead (T.pack $ show ("err-processStack", other))]


actionStack stack = ST.runST $ do
  pointersRef <- ST.newSTRef (Map.empty :: Map T.Text (UF.Point s (T.Text, Int, Maybe Erl)))
  traverse (actionStackOne pointersRef) stack
  pointers <- ST.readSTRef pointersRef
  rawActions <- traverse UF.descriptor pointers
  pure $ Map.map handleAction rawActions

data GoRewrite
  = GoRename T.Text
  | GoLetBind T.Text
  | GoSkip
  | GoInline Erl
  deriving (Show)

handleAction :: (T.Text, Int, Maybe Erl) -> GoRewrite
handleAction (wantVar, readCount, mValue) =
  case readCount of
    0 -> GoSkip
    _ ->
      case mValue of
        Nothing -> GoRename wantVar
        Just value ->
          case valueSize value * readCount < 100 of
            True -> GoInline value
            False -> GoLetBind wantVar

-- TODO[drathier]: calculate approximate size of an erl expression, for inlining purposes
valueSize e = 1 -- 75

data AAction
  = AAlias
  | ADef
  | ARead
  | AWrite Erl

merge (at, arc, av) (bt, brc, bv) =
  ( if T.length at < T.length bt then bt else at
  , arc+brc
  , av<|>bv
  )

actionStackOne pointersRef item = do
  case item of
    SAlias a b -> do
      aRef <- findGroup pointersRef a AAlias
      bRef <- findGroup pointersRef b AAlias
      UF.union'
        aRef bRef (\av bv -> pure (merge av bv))
      pure ()
    SDef a -> do
      findGroup pointersRef a ADef
      pure ()
    SRead a -> do
      -- TODO[drathier]: track number of reads?
      findGroup pointersRef a ARead
      pure ()
    SWrite a v -> do
      -- TODO[drathier]: track number of reads?
      findGroup pointersRef a (AWrite v)
      pure ()


findGroup pointersRef a kind = do
  let kindState =
        case kind of
          AAlias -> (a, 0, Nothing)
          ADef -> (a, 0, Nothing)
          ARead -> (a, 1, Nothing)
          AWrite v -> (a, 0, Just v)

  p <- ST.readSTRef pointersRef
  case Map.lookup a p of
    Just aPoint -> do
      UF.modifyDescriptor aPoint (merge kindState)
      pure aPoint
    Nothing -> do
      aPoint <- UF.fresh kindState
      ST.modifySTRef pointersRef (Map.insert a aPoint)
      pure aPoint

------------------

inlineVarBinds :: [Erl] -> [Erl]
inlineVarBinds erls =
  map inlineVarBindsImpl erls

inlineVarBindsImpl :: Erl -> Erl
-- inlineVarBindsImpl erl@(EFunctionDef _ _ name args _) | runAtom name /= "match" || length args /= 2 = erl
inlineVarBindsImpl erl =
  let (res,state) = runState (collectErl erl) initialDB
      (res2,state2) = runState (replaceErl erl) (processStack (_stack state))
  in
  -- trace (show ("inlineVarBindsImpl", ("rev", reverse $ _stack state), ("processStack", processStack (_stack state)), erl)) $
  res2

collectErl = everywhereOnErlTopDownLeftToRightM collectOnErl

collectOnErl :: Erl -> State DB Erl
collectOnErl e = do
  case e of
    EFunctionDef _ _ _ vs _ -> mapM_ (track e . SDef) vs >> pure e
    EFunFull mFunName binders -> mapM_ (track e . SDef) mFunName >> mapM_ (\(b,_) -> collectBinder b) binders >> pure e

    EVar v -> track e (SRead v) >> pure e
    ELet (EBind (EVar v) (EVar v2)) _ -> track e (SAlias v v2) >> pure e
    ELet (EBind (EVar v) rhs) _ -> track e (SWrite v rhs) >> pure e
    other -> pure other

collectPat = everywhereOnErlTopDownLeftToRightM collectOnPat

collectOnPat :: Erl -> State DB Erl
collectOnPat e = do
  case e of
    EVar v -> track e (SDef v) >> pure e
    other -> pure other

collectBinder :: EFunBinder -> State DB ()
collectBinder binder =
  case binder of
    EFunBinder pats -> do
      mapM_ collectPat pats
      -- mapM_ collectGuard mGuard

-- collectGuard :: Guard -> State DB Guard
-- collectGuard (Guard g) = Guard <$> collectErl g

----

type Replacements = Map T.Text GoRewrite

replaceErl = everywhereOnErlTopDownLeftToRightWithoutEBindPatM replaceOnErl

replaceOnErl :: Erl -> State Replacements Erl
replaceOnErl e = do
  -- ASSUMPTION[drathier]: GoInline never happens to variables directly or indirectly mentioned in SDef.
  db <- get
  let goVarExpr var =
        case Map.lookup var db of
          Nothing -> EVar "missing_dolphin" -- ELet (EVar "goNothing-") (EVar var) -- $  error (show ("InlineLocal.replace.go", ("var", var), ("e", e), ("db", db)))
          Just (GoRename want) -> EVar want
          Just (GoLetBind want) -> EVar want
          Just GoSkip -> EVar "_"
          Just (GoInline rhs) -> rhs
  let goDef a =
        case Map.lookup a db of
          Nothing -> "missing_monkey" -- "_goDefNothing-" <> a -- error (show ("InlineLocal.replace.{goDef,goAtom}", ("atom", a), ("e", e), ("db", db)))
          Just (GoRename want) -> want
          Just (GoLetBind want) -> want
          Just GoSkip -> "_"
          Just (GoInline rhs) -> "_" -- error (show ("InlineLocal.replace.{goDef,goAtom} got GoInline", ("atom", a), ("e", e), ("db", db)))
  let goAtom a = Atom Nothing $ goDef (runAtom a)

      replacePat = everywhereOnErlTopDownLeftToRightWithoutEBindPatM replaceOnPat

      replaceOnPat :: Erl -> State Replacements Erl
      replaceOnPat e = do
        case e of
          EVar v -> pure $ EVar (goDef v)
          other -> pure $ other

      replaceBinder :: EFunBinder -> State Replacements EFunBinder
      replaceBinder binder =
        case binder of
          EFunBinder pats -> do
            pats' <- mapM replacePat pats
            -- mGuard' <- mapM replaceGuard mGuard
            pure (EFunBinder pats')-- mGuard')

      replaceGuard :: Guard -> State Replacements Guard
      replaceGuard (Guard g) = Guard <$> replaceErl g


  case e of
    -- EFunctionDef met mss funName args body | trace (show ("NEEDLE-EFunctionDef", funName)) False -> error "unreachable-drathier-o9238475"
    -- EFunFull mFunName binders | trace (show ("NEEDLE-EFunFull", mFunName)) False -> error "unreachable-drathier-o9238472345"
    -- ELet (EBind (EVar v) _) _ | trace (show ("NEEDLE-ELet", v)) False -> error "unreachable-drathier-1239847"
    -- ELet bind _ | trace (show ("NEEDLE-ELet-bind", bind)) False -> error "unreachable-drathier-1239847"

    EFunctionDef met mss funName args body -> pure $ EFunctionDef met mss funName (map goDef args) body
    EFunFull mFunName binders -> EFunFull (fmap goDef mFunName) <$> mapM (\(b,rhs) -> (,) <$> replaceBinder b <*> pure rhs) binders

    EVar v -> pure $ goVarExpr v
    ELet (EBind (EVar v) (EVar v2)) body -> replaceOnErl $ body
    ELet (EBind (EVar v) rhs) body ->
      replaceOnErl $ case Map.lookup v db of
        Nothing -> EVar "missing_horse" -- ELet (EVar "let0Nothing-") (EVar v) -- $ error (show ("InlineLocal.replace", ("v", v), ("e", e), ("db", db)))
        Just (GoRename want) -> ELet (EBind (EVar want) rhs) body
        Just (GoLetBind want) -> ELet (EBind (EVar want) rhs) body
        Just GoSkip -> body
        Just (GoInline _) -> body

    other -> pure $ other