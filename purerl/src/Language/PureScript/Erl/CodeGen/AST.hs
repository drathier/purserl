{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Data types for the intermediate simplified-Erlang AST
--
module Language.PureScript.Erl.CodeGen.AST where

import Prelude.Compat

import Data.Text (Text)
import Data.Text qualified as T

import Control.Monad.Identity
import Control.Arrow (second)

import Language.PureScript.PSString (PSString)
import Language.PureScript.AST.SourcePos
import Debug.Trace (traceM, trace)

-- |
-- Data type for simplified Erlang expressions
--
data Erl
  -- |
  -- A numeric literal
  --
  = ENumericLiteral (Either Integer Double)
  -- |
  -- A string literal
  --
  | EStringLiteral PSString
  -- |
  -- A char literal
  --
  | ECharLiteral Char
  -- |
  -- An atom literal (possibly qualified a:b)
  --
  | EAtomLiteral Atom
  -- |
  -- A unary operator application
  --
  | EUnary UnaryOperator Erl
  -- |
  -- A binary operator application
  --
  | EBinary BinaryOperator Erl Erl
  -- |
  -- Top-level function definition (over-simplified)
  --
  | EFunctionDef (Maybe EType) (Maybe SourceSpan) Atom [Text] Erl
  -- TODO not really a separate form. and misused
  | EBind Erl Erl
  -- |
  -- A variable
  --
  | EVar Text
  -- |
  -- A function reference f/1
  --
  | EFunRef Atom Int
  -- |
  -- A fun definition
  --
  | EFunFull (Maybe Text) [(EFunBinder, Erl)]
  -- |
  -- Function application
  --
  | EApp AppAnnotation Erl [Erl]
  -- |
  -- Block
  --
  | EBlock [Erl] -- Array should be nonempty
  -- |
  -- An effectful variable bind, separate from EBlock to keep track of which function calls are effectful
  --
  | EAndThen Erl Erl
  -- |
  -- One line in an EBlock, separate from EBlock to avoid having to flatten blocks
  --
  | ELet Erl Erl

  -- |
  -- Tuple literal {a, 1, "C"}
  --
  | ETupleLiteral [Erl]

  | EComment Text

  | EMapLiteral [(Atom, Erl)]

  | EArrayLiteral [Erl]

  | EMapPattern [(Atom, Erl)]

  | EMapUpdate Erl [(Atom,Erl)]

  | EListLiteral [Erl]
  
  | EListCons [Erl] Erl

  | ECaseOf Erl [(EBinder, Erl)]
  -- |
  -- Attribute including raw text between the parens
  --
  | EAttribute PSString PSString
  -- Spec attribute
  | ESpec Atom EType
  | EType Atom [Text] EType
  -- [drathier]: Try
  | ETryAnyAny Erl Erl
  -- [drathier]: raw erlang code
  | ERawErlangSource T.Text [(Atom, Erl)]

  deriving (Show, Eq)

-- | [drathier]: I think this is annotating if a particular App is applying something compiler-generated like a type class dict (SyntheticApp), or is just a normal function call (RegularApp).
data AppAnnotation 
  = RegularApp
  | SyntheticApp
  deriving (Show, Eq)

-- | EVarBind as defined before drathier expanded it to allow more than simple var lhs's
pattern EVarBind :: Text -> Erl -> Erl
pattern EVarBind name e = EBind (EVar name) e

-- | Aliases of EApp with specific args exact-called out and others exact-called after, to width-align pattern matches
-- pattern EApp1 :: Maybe Text -> Erl -> Erl
-- pattern EApp1 appKind modu f instModu instPrefix = EApp

-- pattern EApp3 :: _

pattern EApp1 modu1 f instModu instF a b <- EApp _ (EApp _ (EApp _ (EAtomLiteral (Atom (Just modu1) f)) [EApp _ (EAtomLiteral (Atom (Just instModu) instF)) []]) [a]) [b]
pattern EApp2 modu1 f instModu instF a b <- EApp _ (EApp _ (EAtomLiteral (Atom (Just modu1) f)) [EApp _ (EAtomLiteral (Atom (Just instModu) instF)) []]) [a,b]
-- pattern EApp1 modu1 f instModu instF a b <- EApp _ (EApp _ (EAtomLiteral (Atom (Just modu1) f)) [EApp _ (EAtomLiteral (Atom (Just instModu) instF)) []]) [a, b]
pattern EApp3 modu1 f instModu instF a b <- EApp _ (EAtomLiteral (Atom (Just modu1) f)) [EApp _ (EAtomLiteral (Atom (Just instModu) instF)) [], a, b]

pattern ETrue = EAtomLiteral (Atom Nothing "true")
pattern EFalse = EAtomLiteral (Atom Nothing "false")

pattern EIntLit a = ENumericLiteral (Left a)
pattern ENumLit a = ENumericLiteral (Right a)

pattern ENegate a = EUnary Negate a

-- | Simple 0-arity version of EFun1
pattern EFun0 :: Maybe Text -> Erl -> Erl
pattern EFun0 name e = EFunFull name [(EFunBinder [], e)]

-- | Simple fun definition fun f(X) -> e end (arity 1 with single head with simple variable pattern, name optional)
pattern EFun1 :: Maybe Text -> Text -> Erl -> Erl
pattern EFun1 name var e = EFunFull name [(EFunBinder [EVar var], e)]

extractVars :: [Erl] -> Maybe [Text]
extractVars = traverse var
  where var (EVar x) = Just x
        var _ = Nothing

-- | Simple arity-N version of EFun1
pattern EFunN :: Maybe Text -> [Text] -> Erl -> Erl
pattern EFunN name vars e <- EFunFull name [(EFunBinder (extractVars -> Just vars), e)] where
  EFunN name vars e = EFunFull name [(EFunBinder (map EVar vars), e)]


curriedLambda :: Erl -> [Text] -> Erl
curriedLambda = foldr (EFun1 Nothing)

curriedApp :: [Erl] -> Erl -> Erl
curriedApp = flip (foldl (\fn a -> EApp RegularApp fn [a]))

litAtom :: Text -> Erl
litAtom = EAtomLiteral . Atom Nothing
qualFunCall :: Text -> Text -> [Erl] -> Erl
qualFunCall q t = EApp RegularApp (EAtomLiteral $ Atom (Just q) t)


data EFunBinder
 = EFunBinder [Erl]
 -- [drathier]: using ECaseOf instead of EFunBinder _ (Just _)
 -- = EFunBinder [Erl] (Maybe Guard)

   deriving (Show, Eq)

data EBinder
  = EBinder Erl -- TODO split out literals?
  -- [drathier]: using ECaseOf instead of EGuardedBinder
  -- | EGuardedBinder Erl Guard

  deriving (Show, Eq)

data Guard
  = Guard Erl
  deriving (Show, Eq)

-- | Possibly qualified atom
-- TODO : This is not really an atom, each part is an atom.
data Atom
  = Atom (Maybe Text) Text
  | AtomPS (Maybe Text) PSString
  deriving (Show, Eq, Ord)
-- |
-- Built-in unary operators
--
data UnaryOperator
  -- |
  -- Numeric negation
  --
  = Negate
  -- |
  -- Boolean negation
  --
  | Not
  -- |
  -- Bitwise negation
  --
  | BitwiseNot
  -- |
  -- Numeric unary \'plus\'
  --
  | Positive
  deriving (Show, Eq)

-- |
-- Built-in binary operators
--
data BinaryOperator
  -- |
  -- Numeric addition
  --
  = Add
  -- |
  -- Numeric subtraction
  --
  | Subtract
  -- |
  -- Numeric multiplication
  --
  | Multiply
  -- |
  -- Numeric division (float)
  --
  | FDivide

  -- [drathier]: Purescript euclidian integer division and remainder is not the same as erlang division, so we can't inline it as `div` or `rem` here. See data_euclideanRing@foreign:intDiv and https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf for the various kinds of division.
  -- -- |
  -- -- Numeric division (integer)
  -- --
  -- -- | IDivide
  -- -- |
  -- -- Integer Remainder
  -- --
  -- | IRemainder
  -- |
  -- Float Remainder
  --
  | FRemainder
  -- |
  -- Generic equality test
  --
  | EqualTo
  -- |
  -- Generic inequality test
  --
  | NotEqualTo
  -- |
  -- Generic identical test
  --
  | IdenticalTo
  -- |
  -- Generic non-identical test
  --
  | NotIdenticalTo
  -- |
  -- Numeric less-than
  --
  | LessThan
  -- |
  -- Numeric less-than-or-equal
  --
  | LessThanOrEqualTo
  -- |
  -- Numeric greater-than
  --
  | GreaterThan
  -- |
  -- Numeric greater-than-or-equal
  --
  | GreaterThanOrEqualTo

  -- |
  -- Boolean and
  --
  | And
  -- |
  -- Boolean or
  --
  | Or
  -- |
  -- Boolean short-circuit and
  --
  | AndAlso
  -- |
  -- Boolean short-circuit or
  --
  | OrElse
  -- |
  -- Boolean xor
  --
  | XOr
  -- |
  -- Bitwise and
  --
  | BitwiseAnd
  -- |
  -- Bitwise or
  --
  | BitwiseOr
  -- |
  -- Bitwise xor
  --
  | BitwiseXor
  -- |
  -- Bitwise left shift
  --
  | ShiftLeft
  -- |
  -- Bitwise right shift
  --
  | ShiftRight
  
  -- |
  -- List concatenation (++)
  --
  | ListConcat
  -- |
  -- Binary concatenation (<<A/binary,B/binary>>)
  --
  | BinaryConcat
  -- |
  -- Array concatenation (data_semigroup@foreign:concatArray(A,B))
  --
  | ArrayConcat
  -- |
  -- List subtraction (--)
  --
  | ListSubtract

  deriving (Show, Eq)

-- Simplified Erlang types
data EType
  = TAny
  
  | TNone
  | TPid
  | TPort
  | TReference
  | TNil
  | TAtom (Maybe Atom)
  -- bitstring
  | TFloat
  -- | TFunAny
  | TVar Text
  | TFun [EType] EType
  | TInteger -- no ranges
  | TList EType -- no improper lists
  -- maps
  | TMap (Maybe [(EType, EType)]) 
  | TTuple [EType]
  | TUnion [EType]
  | TRemote Text Text [EType]
  | TAlias Atom [EType]
  deriving (Show, Eq)

everywhereOnErl :: (Erl -> Erl) -> Erl -> Erl
everywhereOnErl f = go
  where
  go :: Erl -> Erl
  go erl =
    case erl of
      EVar {} -> f erl
      EAtomLiteral {} -> f erl
      ENumericLiteral {} -> f erl
      EStringLiteral {} -> f erl
      ECharLiteral {} -> f erl
      EFunRef {} -> f erl
      EComment {} -> f erl
      EAttribute {} -> f erl
      ESpec {} -> f erl
      EType {} -> f erl

      EUnary op e -> f $ EUnary op (go e)
      EBinary op e1 e2 -> f $ EBinary op (go e1) (go e2)
      EFunctionDef t ssann a vs e -> f $ EFunctionDef t ssann a vs (go e)
      EBind x e -> f $ EBind (go x) (go e)
      EFunFull fname args -> f $ EFunFull fname $ map (second go) args
      EApp meta e es -> f $ EApp meta (go e) (map go es)
      EBlock es -> f $ EBlock (map go es)
      ETupleLiteral es -> f $ ETupleLiteral (map go es)
      EArrayLiteral es -> f $ EArrayLiteral (map go es)
      EMapLiteral binds -> f $ EMapLiteral $ map (second go) binds
      EMapPattern binds -> f $ EMapPattern $ map (second go) binds
      EMapUpdate e binds -> f $ EMapUpdate (go e) $ map (second go) binds
      ECaseOf e binds -> f $ ECaseOf (go e) $ map (second go) binds
      EListLiteral es -> f $ EListLiteral (map go es)
      EListCons es e -> f $ EListCons (map go es) (go e)
      ETryAnyAny e1 e2 -> f $ ETryAnyAny (go e1) (go e2)
      EAndThen a b -> f $ EAndThen (go a) (go b)
      ELet a b -> f $ ELet (go a) (go b)
      ERawErlangSource fmt binds -> f $ ERawErlangSource fmt $ map (second go) binds

      -- other -> error (show ("other", other))

everywhereOnErlBottomUpM :: forall m. Monad m => (Erl -> m Erl) -> Erl -> m Erl
everywhereOnErlBottomUpM f expr =
  let go ex =
        case ex of
          EVar {} -> f ex
          EAtomLiteral {} -> f ex
          ENumericLiteral {} -> f ex
          EStringLiteral {} -> f ex
          ECharLiteral {} -> f ex
          EFunRef {} -> f ex
          EComment {} -> f ex
          EAttribute {} -> f ex
          ESpec {} -> f ex
          EType {} -> f ex

          EUnary op e -> f =<< EUnary op <$> go e
          EBinary op e1 e2 -> f =<< EBinary op <$> go e1 <*> go e2
          EFunctionDef t ssann a ss e -> f =<< EFunctionDef t ssann a ss <$> go e
          EBind x e -> f =<< EBind <$> go x <*> go e
          EFunFull fname args -> f =<< EFunFull fname <$> traverse (traverse go) args
          EApp meta e es -> f =<< EApp meta <$> go e <*> traverse go es
          EBlock es -> f =<< EBlock <$> traverse go es
          ETupleLiteral es -> f =<< ETupleLiteral <$> traverse go es
          EArrayLiteral es -> f =<< EArrayLiteral <$> traverse go es
          EMapLiteral binds -> f =<< EMapLiteral <$> traverse (traverse go) binds
          EMapPattern binds -> f =<< EMapPattern <$> traverse (traverse go) binds
          EMapUpdate e binds -> f =<< EMapUpdate <$> go e <*> traverse (traverse go) binds
          ECaseOf e binds -> f =<< ECaseOf <$> go e <*> traverse (traverse go) binds
          EListLiteral es -> f =<< EListLiteral <$> traverse go es
          EListCons es e -> f =<< EListCons <$> traverse go es <*> go e
          ETryAnyAny e1 e2 -> f =<< ETryAnyAny <$> go e1 <*> go e2
          EAndThen a b -> f =<< EAndThen <$> go a <*> go b
          ELet a b -> f =<< ELet <$> go a <*> go b
          ERawErlangSource fmt binds -> f =<< ERawErlangSource fmt <$> traverse (traverse go) binds
  in go expr

everywhereOnErlBottomUpLeftToRightM :: forall m. Monad m => (Erl -> m Erl) -> Erl -> m Erl
everywhereOnErlBottomUpLeftToRightM f expr =
  let go ex =
        case ex of
          EVar {} -> f ex
          EAtomLiteral {} -> f ex
          ENumericLiteral {} -> f ex
          EStringLiteral {} -> f ex
          ECharLiteral {} -> f ex
          EFunRef {} -> f ex
          EComment {} -> f ex
          EAttribute {} -> f ex
          ESpec {} -> f ex
          EType {} -> f ex

          EUnary op e -> do
            e' <- go e
            f (EUnary op e')
          EBinary op e1 e2 -> do
            e1' <- go e1
            e2' <- go e2
            f (EBinary op e1' e2')
          EFunctionDef t ssann a ss e -> do
            e' <- go e
            f (EFunctionDef t ssann a ss e')
          EBind x e -> do
            x' <- go x
            e' <- go e
            f (EBind x' e')
          EFunFull fname args -> do
            args' <- traverse (traverse go) args
            f (EFunFull fname args')
          EApp meta e es -> do
            e' <- go e
            es' <- traverse go es
            f (EApp meta e' es')
          EBlock es -> do
            es' <- traverse go es
            f (EBlock es')
          ETupleLiteral es -> do
            es' <- traverse go es
            f (ETupleLiteral es')
          EArrayLiteral es -> do
            es' <- traverse go es
            f (EArrayLiteral es')
          EMapLiteral binds -> do
            binds' <- traverse (traverse go) binds
            f (EMapLiteral binds')
          EMapPattern binds -> do
            binds' <- traverse (traverse go) binds
            f (EMapPattern binds')
          EMapUpdate e binds -> do
            e' <- go e
            binds' <- traverse (traverse go) binds
            f (EMapUpdate e' binds')
          ECaseOf e binds -> do
            e' <- go e
            binds' <- traverse (traverse go) binds
            f (ECaseOf e' binds')
          EListLiteral es -> do
            es' <- traverse go es
            f (EListLiteral es')
          EListCons es e -> do
            es' <- traverse go es
            e' <- go e
            f (EListCons es' e')
          ETryAnyAny e1 e2 -> do
            e1' <- go e1
            e2' <- go e2
            f (ETryAnyAny e1' e2')
          EAndThen a b -> do
            a' <- go a
            b' <- go b
            f (EAndThen a' b')
          ELet a b -> do
            a' <- go a
            b' <- go b
            f (ELet a' b')
          ERawErlangSource fmt binds -> do
            binds' <- traverse (traverse go) binds
            f (ERawErlangSource fmt binds')
  in go expr

everywhereOnErlTopDownLeftToRightM :: forall m. Monad m => (Erl -> m Erl) -> Erl -> m Erl
everywhereOnErlTopDownLeftToRightM f expr =
  let go exInput = do
        ex <- f exInput
        case ex of
          EVar {} -> pure ex
          EAtomLiteral {} -> pure ex
          ENumericLiteral {} -> pure ex
          EStringLiteral {} -> pure ex
          ECharLiteral {} -> pure ex
          EFunRef {} -> pure ex
          EComment {} -> pure ex
          EAttribute {} -> pure ex
          ESpec {} -> pure ex
          EType {} -> pure ex

          EUnary op e -> do
            e' <- go e
            pure (EUnary op e')
          EBinary op e1 e2 -> do
            e1' <- go e1
            e2' <- go e2
            pure (EBinary op e1' e2')
          EFunctionDef t ssann a ss e -> do
            e' <- go e
            pure (EFunctionDef t ssann a ss e')
          EBind x e -> do
            x' <- go x
            e' <- go e
            pure (EBind x' e')
          EFunFull fname args -> do
            args' <- traverse (traverse go) args
            pure (EFunFull fname args')
          EApp meta e es -> do
            e' <- go e
            es' <- traverse go es
            pure (EApp meta e' es')
          EBlock es -> do
            es' <- traverse go es
            pure (EBlock es')
          ETupleLiteral es -> do
            es' <- traverse go es
            pure (ETupleLiteral es')
          EArrayLiteral es -> do
            es' <- traverse go es
            pure (EArrayLiteral es')
          EMapLiteral binds -> do
            binds' <- traverse (traverse go) binds
            pure (EMapLiteral binds')
          EMapPattern binds -> do
            binds' <- traverse (traverse go) binds
            pure (EMapPattern binds')
          EMapUpdate e binds -> do
            e' <- go e
            binds' <- traverse (traverse go) binds
            pure (EMapUpdate e' binds')
          ECaseOf e binds -> do
            e' <- go e
            binds' <- traverse (traverse go) binds
            pure (ECaseOf e' binds')
          EListLiteral es -> do
            es' <- traverse go es
            pure (EListLiteral es')
          EListCons es e -> do
            es' <- traverse go es
            e' <- go e
            pure (EListCons es' e')
          ETryAnyAny e1 e2 -> do
            e1' <- go e1
            e2' <- go e2
            pure (ETryAnyAny e1' e2')
          EAndThen a b -> do
            a' <- go a
            b' <- go b
            pure (EAndThen a' b')
          ELet a b -> do
            a' <- go a
            b' <- go b
            pure (ELet a' b')
          ERawErlangSource fmt binds -> do
            binds' <- traverse (traverse go) binds
            pure (ERawErlangSource fmt binds')
  in go expr

everywhereOnErlTopDownLeftToRightWithoutEBindPatM :: forall m. Monad m => (Erl -> m Erl) -> Erl -> m Erl
everywhereOnErlTopDownLeftToRightWithoutEBindPatM f expr =
  -- NOTE[drathier]: previous hard-to-spot bug here. Removing an ELet in the body of another ELet makes it really hard to remember to recurse into that newly added thing, and not just its children. Starting a new recursive call in f outside of this function will likely cause double traversals of children. Thus, we return a Maybe to signify if anything changed.
  let go exInput = do
        ex <- f exInput

        case ex of
          EVar {} -> pure ex
          EAtomLiteral {} -> pure ex
          ENumericLiteral {} -> pure ex
          EStringLiteral {} -> pure ex
          ECharLiteral {} -> pure ex
          EFunRef {} -> pure ex
          EComment {} -> pure ex
          EAttribute {} -> pure ex
          ESpec {} -> pure ex
          EType {} -> pure ex

          EUnary op e -> do
            e' <- go e
            pure (EUnary op e')
          EBinary op e1 e2 -> do
            e1' <- go e1
            e2' <- go e2
            pure (EBinary op e1' e2')
          EFunctionDef t ssann a ss e -> do
            e' <- go e
            pure (EFunctionDef t ssann a ss e')
          EBind x e -> do
            x' <- pure x
            e' <- go e
            pure (EBind x' e')
          EFunFull fname args -> do
            args' <- traverse (traverse go) args
            pure (EFunFull fname args')
          EApp meta e es -> do
            e' <- go e
            es' <- traverse go es
            pure (EApp meta e' es')
          EBlock es -> do
            es' <- traverse go es
            pure (EBlock es')
          ETupleLiteral es -> do
            es' <- traverse go es
            pure (ETupleLiteral es')
          EArrayLiteral es -> do
            es' <- traverse go es
            pure (EArrayLiteral es')
          EMapLiteral binds -> do
            binds' <- traverse (traverse go) binds
            pure (EMapLiteral binds')
          EMapPattern binds -> do
            binds' <- traverse (traverse go) binds
            pure (EMapPattern binds')
          EMapUpdate e binds -> do
            e' <- go e
            binds' <- traverse (traverse go) binds
            pure (EMapUpdate e' binds')
          ECaseOf e binds -> do
            e' <- go e
            binds' <- traverse (traverse go) binds
            pure (ECaseOf e' binds')
          EListLiteral es -> do
            es' <- traverse go es
            pure (EListLiteral es')
          EListCons es e -> do
            es' <- traverse go es
            e' <- go e
            pure (EListCons es' e')
          ETryAnyAny e1 e2 -> do
            e1' <- go e1
            e2' <- go e2
            pure (ETryAnyAny e1' e2')
          EAndThen a b -> do
            a' <- go a
            b' <- go b
            pure (EAndThen a' b')
          ELet a b -> do
            a' <- go a
            b' <- go b
            pure (ELet a' b')
          ERawErlangSource fmt binds -> do
            binds' <- traverse (traverse go) binds
            pure (ERawErlangSource fmt binds')
  in go expr


everywhereOnErlTopDown :: (Erl -> Erl) -> Erl -> Erl
everywhereOnErlTopDown f = runIdentity . everywhereOnErlTopDownM (Identity . f)

everywhereOnErlTopDownM :: forall m. (Monad m) => (Erl -> m Erl) -> Erl -> m Erl
everywhereOnErlTopDownM f = f >=> go
  where
  f' = f >=> go

  fargs :: [(x, Erl)] -> m [(x, Erl)]
  fargs = traverse (sequence . second f')

  go (EUnary op e) = EUnary op <$> f' e
  go (EBinary op e1 e2) = EBinary op <$> f' e1 <*> f' e2
  go (EFunctionDef t ssann a ss e) = EFunctionDef t ssann a ss <$> f' e
  go (EBind x e) = EBind <$> f' x <*> f' e
  go (EFunFull fname args) = EFunFull fname <$> fargs args
  go (EApp meta e es) = EApp meta <$> f' e <*> traverse f' es
  go (EBlock es) = EBlock <$> traverse f' es
  go (ETupleLiteral es) = ETupleLiteral <$> traverse f' es
  go (EArrayLiteral es) = EArrayLiteral <$> traverse f' es
  go (EMapLiteral binds) = EMapLiteral <$> fargs binds
  go (EMapPattern binds) = EMapPattern <$> fargs binds
  go (EMapUpdate e binds) = EMapUpdate <$> f' e <*> fargs binds
  go (ECaseOf e binds) = ECaseOf <$> f' e <*> fargs binds
  go (EListLiteral es) = EListLiteral <$> traverse f' es
  go (EListCons es e) = EListCons <$> traverse f' es <*> f' e
  go (ETryAnyAny e1 e2) = ETryAnyAny <$> f' e1 <*> f' e2
  go (EAndThen a b) = EAndThen <$> f' a <*> f' b
  go (ELet a b) = ELet <$> f' a <*> f' b
  go (ERawErlangSource fmt binds) = ERawErlangSource fmt <$> fargs binds

  go other = f other

-- Sorry. Really want a type that allows "child context" under binders etc
everywhereOnErlTopDownMThen :: forall m. (Monad m) => (Erl -> m (Erl, Erl -> m Erl)) -> Erl -> m Erl
everywhereOnErlTopDownMThen f = f'
  where
  f' e = do
    (x, f1) <- f e
    y <- go x
    f1 y

  fargs :: [(x, Erl)] -> m [(x, Erl)]
  fargs = traverse (sequence . second f')

  go (EUnary op e) = EUnary op <$> f' e
  go (EBinary op e1 e2) = EBinary op <$> f' e1 <*> f' e2
  go (EFunctionDef t ssann a ss e) = EFunctionDef t ssann a ss <$> f' e
  go (EBind x e) = EBind <$> f' x <*> f' e
  go (EFunFull fname args) = EFunFull fname <$> fargs args
  go (EApp meta e es) = EApp meta <$> f' e <*> traverse f' es
  go (EBlock es) = EBlock <$> traverse f' es
  go (ETupleLiteral es) = ETupleLiteral <$> traverse f' es
  go (EArrayLiteral es) = EArrayLiteral <$> traverse f' es
  go (EMapLiteral binds) = EMapLiteral <$> fargs binds
  go (EMapPattern binds) = EMapPattern <$> fargs binds
  go (EMapUpdate e binds) = EMapUpdate <$> f' e <*> fargs binds
  go (ECaseOf e binds) = ECaseOf <$> f' e <*> fargs binds
  go (EListLiteral es) = EListLiteral <$> traverse f' es
  go (EListCons es e) = EListCons <$> traverse f' es <*> f' e
  go (ETryAnyAny e1 e2) = ETryAnyAny <$> f' e1 <*> f' e2
  go (EAndThen a b) = EAndThen <$> f' a <*> f' b
  go (ELet a b) = ELet <$> f' a <*> f' b
  go (ERawErlangSource fmt binds) = ERawErlangSource fmt <$> fargs binds
  go other = fst <$> f other
  
everything :: forall r. (r -> r -> r) -> (Erl -> r) -> Erl -> r
everything (<>.) f = go
  where
  go :: Erl -> r
  go e0@(EUnary _ e) = f e0 <>. go e
  go e0@(EBinary _ e1 e2) = f e0 <>. go e1 <>. go e2
  go e0@(EFunctionDef _ _ _ _ e) = f e0 <>. go e
  go e0@(EBind x e) = f e0 <>. go x <>. go e
  go e0@(EFunFull _ args) = foldl (<>.) (f e0) (map (go . snd) args)
  go e0@(EApp _ e es) = foldl (<>.) (f e0 <>. go e) (map go es)
  go e0@(EBlock es) = foldl (<>.) (f e0) (map go es)
  go e0@(ETupleLiteral es) = foldl (<>.) (f e0) (map go es)
  go e0@(EArrayLiteral es) = foldl (<>.) (f e0) (map go es)
  go e0@(EMapLiteral binds) = foldl (<>.) (f e0) (map (go . snd) binds)
  go e0@(EMapPattern binds) = foldl (<>.) (f e0) (map (go . snd) binds)
  go e0@(EMapUpdate e binds) = foldl (<>.) (f e0 <>. go e) (map (go . snd) binds)
  go e0@(ECaseOf e binds) = foldl (<>.) (f e0 <>. go e) (map (go . snd) binds)
  go e0@(EListLiteral es) = foldl (<>.) (f e0) (map go es)
  go e0@(EListCons es e) = foldl (<>.) (f e0) (map go $ es <> [e])
  go e0@(ETryAnyAny e1 e2) = f e0 <>. go e1 <>. go e2
  go e0@(EAndThen a b) = f e0 <>. go a <>. go b
  go e0@(ELet a b) = f e0 <>. go a <>. go b
  go e0@(ERawErlangSource _ binds) = foldl (<>.) (f e0) (map (go . snd) binds)
  -- go other = f other

  go e0@(EVar {}) = f e0
  go e0@(EAtomLiteral {}) = f e0
  go e0@(ENumericLiteral {}) = f e0
  go e0@(EStringLiteral {}) = f e0
  go e0@(ECharLiteral {}) = f e0
  go e0@(EFunRef {}) = f e0
  go e0@(EComment {}) = f e0
  go e0@(EAttribute {}) = f e0
  go e0@(ESpec {}) = f e0
  go e0@(EType {}) = f e0

