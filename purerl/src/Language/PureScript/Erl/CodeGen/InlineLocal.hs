-- |
-- This module optimizes code in the simplified-Erlang intermediate representation.
--
-- The following optimizations are supported:
--
--  * Inlining of (>>=) and ret for the Eff monad
--
module Language.PureScript.Erl.CodeGen.InlineLocal (inlineVarBinds, inlineVarBind) where

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
import Data.Function ((&))

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
  let world = findExternalReads Map.empty cs in
  case world of
    [] ->
      let cs2 = map SDef world <> cs in
      let actions = actionStack cs2 in
      -- trace (show ("processStack", ("cs", cs), ("actions", actions)))
      -- trace (show ("processStack", ("world", world), ("cs2", cs2), ("actions", actions)))
      Just actions
    _ ->
      -- trace (show ("world not empty, we don't handle renames (should use the world var name when renaming), so skipping", world)) $
      Nothing

findExternalReads defined stack =
  let def a = Map.insertWith (||) a True defined in
  let read a = Map.insertWith (||) a False defined in
  case stack of
    SAlias a _ : rest -> findExternalReads (def a) rest
    SWrite a _ : rest -> findExternalReads (def a) rest
    SDef a : rest -> findExternalReads (def a) rest
    SRead a : rest -> findExternalReads (read a) rest
    [] -> defined & Map.toList & filter (\(k,v) -> v == False) & map (\(k,v) -> k)

compressStack stack =
  case stack of
    SAlias a b : SRead a1 : SRead b1 : rest | a == a1, b == b1 -> SAlias a b : compressStack rest
    SWrite a b : SRead a1 : rest | a == a1 -> SWrite a b : compressStack rest
    SDef a : rest -> SDef a : compressStack rest
    SRead a : rest -> SRead a : compressStack rest
    [] -> []


actionStack stack = ST.runST $ do
  pointersRef <- ST.newSTRef (Map.empty :: Map T.Text (UF.Point s (Identifier, Int, Maybe Erl)))
  pointersSeenRef <- ST.newSTRef []
  traverse (actionStackOne (pointersRef, pointersSeenRef)) stack
  pointers <- ST.readSTRef pointersRef
  rawActions <- traverse UF.descriptor pointers
  pure $ Map.map handleAction rawActions

data GoRewrite
  = GoRename T.Text
  | GoLetBind T.Text
  | GoSkip
  | GoInline Erl
  | GoLeaveAsIs
  deriving (Show)

data Identifier
  = IdDefOnce T.Text
  | IdDefMultiple [T.Text]
  | IdUndef T.Text


handleAction :: (Identifier, Int, Maybe Erl) -> GoRewrite
handleAction (wantVar2, readCount, mValue) =
  case wantVar2 of
    _ | 0 == readCount -> GoSkip
    IdDefMultiple _ -> GoLeaveAsIs
    IdUndef wantVar -> handleAction2 wantVar readCount mValue
    IdDefOnce wantVar -> handleAction2 wantVar readCount mValue

handleAction2 wantVar readCount mValue =
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
  deriving (Show)

merge (at, arc, av) (bt, brc, bv) =
  ( -- at <> "___" <> bt --
    -- if T.length at < T.length bt then bt else at
    case (at, bt) of
      (IdDefOnce a, IdDefOnce b) -> IdDefMultiple [a,b]
      (IdDefMultiple a, IdDefMultiple b) -> IdDefMultiple (a <> b)
      (IdDefMultiple a, IdDefOnce b) -> IdDefMultiple (a <> [b])
      (IdDefOnce a, IdDefMultiple b) -> IdDefMultiple (a:b)
      --
      (IdDefMultiple a, IdUndef b) -> IdDefMultiple a
      (IdUndef a, IdDefMultiple b) -> IdDefMultiple b
      --
      (IdDefOnce a, IdUndef b) -> IdDefOnce (if T.length a < T.length b then b else a)
      (IdUndef a, IdDefOnce b) -> IdDefOnce (if T.length a < T.length b then b else a)
      --
      (IdUndef a, IdUndef b) -> IdUndef (if T.length a < T.length b then b else a)
  , arc+brc
  , av<|>bv
  )

actionStackOne pointersRef item = do
  case item of
    SAlias a b -> do
      aRef <- findGroup False pointersRef a AAlias
      bRef <- findGroup False pointersRef b AAlias
      UF.union'
        aRef bRef (\av bv -> pure (merge av bv))
      pure ()
    SDef a -> do
      findGroup True pointersRef a ADef
      pure ()
    SRead a -> do
      -- TODO[drathier]: track number of reads?
      findGroup False pointersRef a ARead
      pure ()
    SWrite a v -> do
      -- TODO[drathier]: track number of reads?
      findGroup True pointersRef a (AWrite v)
      pure ()


findGroup isWrite (pointersRef, pointersSeenRef) a kind = do
  let kindState =
        case kind of
          AAlias -> (IdUndef a, 0, Nothing)
          ADef -> (IdDefOnce a, 0, Nothing)
          ARead -> (IdUndef a, 1, Nothing)
          AWrite v -> (IdDefOnce a, 0, Just v)

  -- -- pop any old variable with same name if this is a write
  -- p <- ST.readSTRef pointersRef
  -- case Map.lookup a p of
  --   Just aPoint | isWrite -> do
  --     ST.modifySTRef pointersSeenRef (\v -> aPoint : v)
  --     ST.modifySTRef pointersRef (Map.delete a)
  --     pure ()
  --   _ -> do
  --     pure ()

  -----------------

  p <- ST.readSTRef pointersRef
  --
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
  map inlineVarBind erls

inlineVarBind :: Erl -> Erl
inlineVarBind erl =
  let (res,state) = runState (collectErl erl) initialDB in
  case (processStack (_stack state)) of
    Nothing -> erl
    Just stack ->
      let
        (res2,state2) = runState (replaceErl erl) stack
      in
      -- trace (show ("inlineVarBind", ("rev", reverse $ _stack state), ("processStack", processStack (_stack state)), ("erl", erl), ("res2", res2))) $
      res2

collectErl = everywhereOnErlTopDownLeftToRightM collectOnErl

collectOnErl :: Erl -> State DB Erl
collectOnErl e = do
  case e of
    EFunctionDef _ _ _ vs _ -> mapM_ (track e . SDef) vs >> pure e
    EFunFull mFunName binders -> mapM_ (track e . SDef) mFunName >> mapM_ (\(b,_) -> collectBinder b) binders >> pure e

    EVar v | v /= "_" -> track e (SRead v) >> pure e
    ELet (EBind (EVar v) (EVar v2)) _ | v /= "_" -> track e (SAlias v v2) >> pure e
    ELet (EBind (EVar v) rhs) _ | v /= "_" -> track e (SWrite v rhs) >> pure e
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
          Nothing | var == "_" -> EVar "_"
          Nothing -> EVar ("missing_dolphin_" <> var) -- ELet (EVar "goNothing-") (EVar var) -- $  error (show ("InlineLocal.replace.go", ("var", var), ("e", e), ("db", db)))
          Just (GoRename want) -> EVar want
          Just (GoLetBind want) -> EVar want
          Just GoSkip -> EVar "_"
          Just (GoInline rhs) -> rhs
          Just GoLeaveAsIs -> EVar var
  let goDef a =
        case Map.lookup a db of
          Nothing -> "missing_monkey" -- "_goDefNothing-" <> a -- error (show ("InlineLocal.replace.{goDef,goAtom}", ("atom", a), ("e", e), ("db", db)))
          Just (GoRename want) -> want
          Just (GoLetBind want) -> want
          Just GoSkip -> "_"
          Just (GoInline rhs) -> "_" -- error (show ("InlineLocal.replace.{goDef,goAtom} got GoInline", ("atom", a), ("e", e), ("db", db)))
          Just GoLeaveAsIs -> a
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

    ECaseOf cond binders ->
      ECaseOf cond <$> mapM (\(EBinder p, rhs) -> do
        p2 <- replaceErl p
        pure (EBinder p2, rhs)) binders

    EVar v -> pure $ goVarExpr v
    ELet (EBind (EVar v) (EVar v2)) body -> replaceOnErl $ body
    ELet (EBind (EVar v) rhs) body ->
      case Map.lookup v db of
        Nothing -> pure $ EVar "missing_horse" -- ELet (EVar "let0Nothing-") (EVar v) -- $ error (show ("InlineLocal.replace", ("v", v), ("e", e), ("db", db)))
        Just (GoRename want) -> replaceOnErl $ ELet (EBind (EVar want) rhs) body
        Just (GoLetBind want) -> replaceOnErl $ ELet (EBind (EVar want) rhs) body
        Just GoSkip -> replaceOnErl $ body
        Just (GoInline _) -> replaceOnErl $ body
        Just GoLeaveAsIs -> pure e

    other -> pure $ other