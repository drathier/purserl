-- |
-- CoreFn traversal helpers
--
module Language.PureScript.CoreFn.Traversals where

import Prelude

import Control.Arrow (second, (***), (+++))
import Data.Bitraversable (bitraverse)

import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.CoreFn.Binders (Binder(..))
import Language.PureScript.CoreFn.Expr (Bind(..), CaseAlternative(..), Expr(..))
import Language.PureScript.Names (Ident, ProperName, ProperNameType(..), Qualified(..))

everywhereOnValues :: (Bind a -> Bind a) ->
                      (Expr a -> Expr a) ->
                      (Binder a -> Binder a) ->
                      (Bind a -> Bind a, Expr a -> Expr a, Binder a -> Binder a)
everywhereOnValues f g h = (f', g', h')
  where
  f' (NonRec a name e) = f (NonRec a name (g' e))
  f' (Rec es) = f (Rec (map (second g') es))

  g' (Literal ann e) = g (Literal ann (handleLiteral g' e))
  g' (Accessor ann prop e) = g (Accessor ann prop (g' e))
  g' (ObjectUpdate ann obj copy vs) = g (ObjectUpdate ann (g' obj) copy (map (fmap g') vs))
  g' (Abs ann name e) = g (Abs ann name (g' e))
  g' (App ann v1 v2) = g (App ann (g' v1) (g' v2))
  g' (Case ann vs alts) = g (Case ann (map g' vs) (map handleCaseAlternative alts))
  g' (Let ann ds e) = g (Let ann (map f' ds) (g' e))
  g' e = g e

  h' (LiteralBinder a b) = h (LiteralBinder a (handleLiteral h' b))
  h' (NamedBinder a name b) = h (NamedBinder a name (h' b))
  h' (ConstructorBinder a q1 q2 bs) = h (ConstructorBinder a q1 q2 (map h' bs))
  h' b = h b

  handleCaseAlternative ca =
    ca { caseAlternativeBinders = map h' (caseAlternativeBinders ca)
       , caseAlternativeResult = (map (g' *** g') +++ g') (caseAlternativeResult ca)
       }

  handleLiteral :: (a -> a) -> Literal a -> Literal a
  handleLiteral i (ArrayLiteral ls) = ArrayLiteral (map i ls)
  handleLiteral i (ObjectLiteral ls) = ObjectLiteral (map (fmap i) ls)
  handleLiteral _ other = other

-- |
-- Apply the provided functions to the top level of AST nodes.
--
-- This function is useful as a building block for recursive functions, but
-- doesn't actually recurse itself.
--
traverseCoreFn
  :: forall f a
   . Applicative f
  => (Bind a -> f (Bind a))
  -> (Expr a -> f (Expr a))
  -> (Binder a -> f (Binder a))
  -> (CaseAlternative a -> f (CaseAlternative a))
  -> (Bind a -> f (Bind a), Expr a -> f (Expr a), Binder a -> f (Binder a), CaseAlternative a -> f (CaseAlternative a))
traverseCoreFn f g h i = (f', g', h', i')
  where
  f' (NonRec a name e) = NonRec a name <$> g e
  f' (Rec es) = Rec <$> traverse (traverse g) es

  g' (Literal ann e) = Literal ann <$> handleLiteral g e
  g' (Accessor ann prop e) = Accessor ann prop <$> g e
  g' (ObjectUpdate ann obj copy vs) = (\obj' -> ObjectUpdate ann obj' copy) <$> g obj <*> traverse (traverse g) vs
  g' (Abs ann name e) = Abs ann name <$> g e
  g' (App ann v1 v2) = App ann <$> g v1 <*> g v2
  g' (Case ann vs alts) = Case ann <$> traverse g vs <*> traverse i alts
  g' (Let ann ds e) = Let ann <$> traverse f ds <*> g e
  g' e = pure e

  h' (LiteralBinder a b) = LiteralBinder a <$> handleLiteral h b
  h' (NamedBinder a name b) = NamedBinder a name <$> h b
  h' (ConstructorBinder a q1 q2 bs) = ConstructorBinder a q1 q2 <$> traverse h bs
  h' b = pure b

  i' ca = CaseAlternative <$> traverse h (caseAlternativeBinders ca) <*> bitraverse (traverse $ bitraverse g g) g (caseAlternativeResult ca)

  handleLiteral withItem = \case
    ArrayLiteral ls -> ArrayLiteral <$> traverse withItem ls
    ObjectLiteral ls -> ObjectLiteral <$> traverse (traverse withItem) ls
    other -> pure other



--------------------------------------------------------------------------------------------------------



traverseCoreFnFull
  :: forall f a
   . Monad f
  => (forall b. f b -> f b)
  -> (Bind a -> f (Bind a))
  -> (Expr a -> f (Expr a))
  -> (Binder a -> f (Binder a))
  -> (CaseAlternative a -> f (CaseAlternative a))
  -> (Literal (Expr a) -> f (Literal (Expr a)))
  -> (Literal (Binder a) -> f (Literal (Binder a)))
  -> (Ident -> f Ident)
  -> ( Bind a -> f (Bind a)
     , Expr a -> f (Expr a)
     , Binder a -> f (Binder a)
     , CaseAlternative a -> f (CaseAlternative a)
     , Literal (Expr a) -> f (Literal (Expr a))
     , Literal (Binder a) -> f (Literal (Binder a))
     , Ident -> f Ident
     )
traverseCoreFnFull isolated bindF exprF binderF caseAltF litExprF litBinderF identF =
  (goBind, goExpr, goBinder, goCaseAlt, goLitExpr, goLitBinder, goIdent)
  where
    qIdentF = pure

    goBind :: Bind a -> f (Bind a)
    goBind b =
      bindF b >>=
      \b -> case b of
        NonRec ann ident expr ->
          NonRec <$> pure ann <*> goIdent ident <*> goExpr expr
        Rec bindings -> do
          Rec <$>
            traverse (\((ann, ident), expr) -> do
              isolated $ (,) <$> ((,) <$> pure ann <*> goIdent ident) <*> goExpr expr
            ) bindings

    goExpr :: Expr a -> f (Expr a)
    goExpr expr =
      exprF expr >>=
      \expr -> case expr of
        Literal ann lit ->
          Literal <$> pure ann <*> goLitExpr lit
        Constructor ann typeName ctorName fields ->
          Constructor <$> pure ann <*> pure typeName <*> pure ctorName <*> traverse goIdent fields
        Accessor ann prop expr ->
          Accessor <$> pure ann <*> pure prop <*> goExpr expr
        ObjectUpdate ann obj mCopy fields ->
          ObjectUpdate <$> pure ann <*> goExpr obj <*> pure mCopy <*> traverse (\(k, v) -> (,) <$> pure k <*> goExpr v) fields
        Abs ann ident body ->
          Abs <$> pure ann <*> goIdent ident <*> goExpr body
        App ann fn arg ->
          App <$> pure ann <*> goExpr fn <*> goExpr arg
        Var ann qIdent ->
          Var <$> pure ann <*> goQIdent goIdent qIdent
        Case ann cases alts ->
          Case <$> pure ann <*> traverse goExpr cases <*> traverse goCaseAlt alts
        Let ann binds body ->
          Let <$> pure ann <*> traverse goBind binds <*> goExpr body

    goQIdent :: (ident -> f ident) -> Qualified ident -> f (Qualified ident)
    goQIdent f qi =
      qIdentF qi >>=
        \qi -> case qi of
          Qualified qualifiedBy a ->
            Qualified <$> pure qualifiedBy <*> f a

    goBinder :: Binder a -> f (Binder a)
    goBinder b =
      binderF b >>=
      \b -> case b of
        NullBinder ann ->
          pure $ NullBinder ann
        LiteralBinder ann lit ->
          LiteralBinder <$> pure ann <*> goLitBinder lit
        VarBinder ann ident ->
          VarBinder <$> pure ann <*> goIdent ident
        ConstructorBinder ann qType qCtor binders ->
          ConstructorBinder <$> pure ann <*> pure qType <*> pure qCtor <*> traverse goBinder binders
        NamedBinder ann ident binder ->
          NamedBinder <$> pure ann <*> goIdent ident <*> goBinder binder

    goCaseAlt :: CaseAlternative a -> f (CaseAlternative a)
    goCaseAlt c =
      isolated $
      caseAltF c >>=
      \c -> case c of
        CaseAlternative binders result ->
          CaseAlternative <$> traverse goBinder binders <*> goResult result
            where
              goResult (Left guards) = Left <$> traverse (\(guard, expr) -> (,) <$> goExpr guard <*> goExpr expr) guards
              goResult (Right expr) = Right <$> goExpr expr

    goLitExpr :: Literal (Expr a) -> f (Literal (Expr a))
    goLitExpr le =
      litExprF le >>=
      \le -> case le of
        NumericLiteral n ->
          pure $ NumericLiteral n
        StringLiteral s ->
          pure $ StringLiteral s
        CharLiteral c ->
          pure $ CharLiteral c
        BooleanLiteral b ->
          pure $ BooleanLiteral b
        ArrayLiteral xs ->
          ArrayLiteral <$> traverse goExpr xs
        ObjectLiteral fields ->
          ObjectLiteral <$> traverse (\(k, v) -> (,) <$> pure k <*> goExpr v) fields

    goLitBinder :: Literal (Binder a) -> f (Literal (Binder a))
    goLitBinder lb =
      litBinderF lb >>=
      \lb -> case lb of
        NumericLiteral n ->
          pure $ NumericLiteral n
        StringLiteral s ->
          pure $ StringLiteral s
        CharLiteral c ->
          pure $ CharLiteral c
        BooleanLiteral b ->
          pure $ BooleanLiteral b
        ArrayLiteral xs ->
          ArrayLiteral <$> traverse goBinder xs
        ObjectLiteral fields ->
          ObjectLiteral <$> traverse (\(k, v) -> (,) <$> pure k <*> goBinder v) fields

    goIdent :: Ident -> f Ident
    goIdent = identF
