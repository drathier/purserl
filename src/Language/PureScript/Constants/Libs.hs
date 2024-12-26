{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Various constants which refer to things in the Prelude and other core libraries
module Language.PureScript.Constants.Libs where

import Protolude qualified as P

import Data.String (IsString)
import Language.PureScript.PSString (PSString)
import Language.PureScript.Constants.TH qualified as TH

-- Core lib values

stRefValue :: forall a. IsString a => a
stRefValue = "value"

-- Type Class Dictionary Names

data EffectDictionaries = EffectDictionaries
  { edApplicativeDict :: PSString
  , edBindDict :: PSString
  , edMonadDict :: PSString
  , edWhile :: PSString
  , edUntil :: PSString
  }

effDictionaries :: EffectDictionaries
effDictionaries = EffectDictionaries
  { edApplicativeDict = "applicativeEff"
  , edBindDict = "bindEff"
  , edMonadDict = "monadEff"
  , edWhile = "whileE"
  , edUntil = "untilE"
  }

effectDictionaries :: EffectDictionaries
effectDictionaries = EffectDictionaries
  { edApplicativeDict = "applicativeEffect"
  , edBindDict = "bindEffect"
  , edMonadDict = "monadEffect"
  , edWhile = "whileE"
  , edUntil = "untilE"
  }

stDictionaries :: EffectDictionaries
stDictionaries = EffectDictionaries
  { edApplicativeDict = "applicativeST"
  , edBindDict = "bindST"
  , edMonadDict = "monadST"
  , edWhile = "while"
  , edUntil = "until"
  }

$(TH.declare do

  -- purescript-prelude
  
  TH.mod "Control.Apply" do
    TH.asIdent do TH.asString do TH.var "apply"

  TH.mod "Control.Applicative" do
    TH.asIdent do TH.asPair do TH.asString do TH.var "pure"

  TH.mod "Control.Bind" do
    TH.asPair do
      TH.asString do
        TH.var "bind"
        TH.cls "Discard" ; TH.var "discard"

      TH.var "discardUnit"

  TH.mod "Control.Category" do
    TH.asPair do
      TH.asIdent do TH.var "identity"

      TH.var "categoryFn"

  TH.mod "Control.Semigroupoid" do
    TH.asPair do
      TH.vars ["compose", "composeFlipped"]
      TH.var "semigroupoidFn"

  TH.mod "Data.Bounded" do
    TH.asPair do
      TH.vars ["bottom", "top"]
      TH.var "boundedBoolean"

  TH.mod "Data.Eq" do
    TH.cls "Eq" ; TH.asIdent do TH.asPair do TH.asString do TH.var "eq"
    TH.cls "Eq1" ; TH.asIdent do TH.asString do TH.var "eq1"
    TH.asPair do
      TH.var "notEq"

      TH.var "eqBoolean"
      TH.var "eqChar"
      TH.var "eqInt"
      TH.var "eqNumber"
      TH.var "eqString"

  TH.mod "Data.EuclideanRing" do
    TH.asPair do
      TH.var "div"

      TH.var "euclideanRingNumber"

  TH.mod "Data.Function" do
    TH.asIdent do
      TH.prefixWith "function" do TH.vars ["apply", "applyFlipped"]
      TH.var "const"
      TH.var "flip"

  TH.mod "Data.Functor" do
    TH.cls "Functor"
    TH.asIdent do TH.asString do TH.vars ["map", "functorArray"]

  TH.mod "Data.Generic.Rep" do
    TH.cls "Generic" ; TH.asIdent do TH.vars ["from", "to"]
    TH.ntys ["Argument", "Constructor", "NoArguments", "NoConstructors", "Product"]
    TH.dty "Sum" ["Inl", "Inr"]

  TH.mod "Data.HeytingAlgebra" do
    TH.asPair do
      TH.asIdent do TH.vars ["conj", "disj", "not"]

      TH.var "heytingAlgebraBoolean"

  TH.mod "Data.Monoid" do
    TH.asIdent do TH.var "mempty"

  TH.mod "Data.Ord" do
    TH.cls "Ord" ; TH.asIdent do TH.asString do TH.var "compare"
    TH.cls "Ord1" ; TH.asIdent do TH.asString do TH.var "compare1"
    TH.asPair do
      TH.vars ["greaterThan", "greaterThanOrEq", "lessThan", "lessThanOrEq"]

      TH.var "ordBoolean"
      TH.var "ordChar"
      TH.var "ordInt"
      TH.var "ordNumber"
      TH.var "ordString"

  TH.mod "Data.Ordering" do
    TH.dty "Ordering" ["EQ", "GT", "LT"]

  TH.mod "Data.Reflectable" do
    TH.cls "Reflectable"

  TH.mod "Data.Ring" do
    TH.asPair do
      TH.asString do TH.vars ["negate", "sub"]

      TH.var "ringInt"
      TH.var "ringNumber"

  TH.mod "Data.Semigroup" do
    TH.asPair do
      TH.asIdent do TH.var "append"

      TH.var "semigroupString"

  TH.mod "Data.Semiring" do
    TH.asPair do
      TH.vars ["add", "mul", "one", "zero"]

      TH.var "semiringInt"
      TH.var "semiringNumber"

  TH.mod "Data.Symbol" do
    TH.cls "IsSymbol"

  -- purescript-arrays

  TH.mod "Data.Array" do
    TH.asPair do TH.var "unsafeIndex"

  -- purescript-bifunctors

  TH.mod "Data.Bifunctor" do
    TH.cls "Bifunctor" ; TH.asIdent do TH.asString do TH.var "bimap"
    TH.asIdent do TH.vars ["lmap", "rmap"]

  -- purescript-contravariant

  TH.mod "Data.Functor.Contravariant" do
    TH.cls "Contravariant" ; TH.asIdent do TH.asString do TH.var "cmap"

  -- purescript-eff

  TH.mod "Control.Monad.Eff" (P.pure ())

  TH.mod "Control.Monad.Eff.Uncurried" do
    TH.asPair do TH.vars ["mkEffFn", "runEffFn"]

  -- purescript-effect

  TH.mod "Effect" (P.pure ())

  TH.mod "Effect.Uncurried" do
    TH.asPair do TH.vars ["mkEffectFn", "runEffectFn"]

  -- purescript-foldable-traversable

  TH.mod "Data.Bifoldable" do
    TH.cls "Bifoldable" ; TH.asIdent do TH.asString do TH.vars ["bifoldMap", "bifoldl", "bifoldr"]

  TH.mod "Data.Bitraversable" do
    TH.cls "Bitraversable" ; TH.asString do TH.asIdent (TH.var "bitraverse"); TH.var "bisequence"
    TH.asIdent do
      TH.vars ["ltraverse", "rtraverse"]

  TH.mod "Data.Foldable" do
    TH.cls "Foldable" ; TH.asIdent do TH.asString do TH.vars ["foldMap", "foldl", "foldr"]

  TH.mod "Data.Traversable" do
    TH.cls "Traversable" ; TH.asString do TH.asIdent (TH.var "traverse") ; TH.var "sequence"

  -- purescript-functions

  TH.mod "Data.Function.Uncurried" do
    TH.asPair do TH.asString do TH.vars ["mkFn", "runFn"]

  -- purescript-integers

  TH.mod "Data.Int.Bits" do
    TH.asPair do
      TH.var "and"
      TH.var "complement"
      TH.var "or"
      TH.var "shl"
      TH.var "shr"
      TH.var "xor"
      TH.var "zshr"

  -- purescript-newtype

  TH.mod "Data.Newtype" do
    TH.cls "Newtype"

  -- purescript-partial

  TH.mod "Partial.Unsafe" do
    TH.asIdent do TH.asPair do TH.var "unsafePartial"

  -- purescript-profunctor

  TH.mod "Data.Profunctor" do
    TH.cls "Profunctor" ; TH.asIdent do TH.asString do TH.var "dimap"
    TH.asIdent do
      TH.var "lcmap"
      TH.prefixWith "profunctor" do TH.var "rmap"

  -- purescript-st

  TH.mod "Control.Monad.ST.Internal" do
    TH.asPair do TH.vars ["modify", "new", "read", "run", "write"]

  TH.mod "Control.Monad.ST.Uncurried" do
    TH.asPair do TH.vars ["mkSTFn", "runSTFn"]

  -- purescript-unsafe-coerce

  TH.mod "Unsafe.Coerce" do
    TH.asPair do TH.var "unsafeCoerce"

  -- [drathier]: purserl-specific modules

  TH.mod "Array" do
    TH.prefixWith "array" do
      TH.asIdent do
        TH.var "map"
        TH.var "bind"
        TH.var "pure"

        -- TODO: semigroup, monoid

  TH.mod "E" do
    TH.prefixWith "e" do
      TH.asIdent do
        TH.var "map"
        TH.var "bind"
        TH.var "pure"

  -- TH.mod "Effect" do
  --   -- TH.prefixWith "effect" do
  --     TH.asIdent do
  --       TH.var "applicativeEffect"
  --       TH.var "applyEffect"
  --       TH.var "bindEffect"
  --       TH.var "functorEffect"
  --       TH.var "monadEffect"
  --       TH.var "monoidEffect"
  --       TH.var "semigroupEffect"

  TH.mod "Either" do
    TH.asIdent do TH.asString do TH.var "functorEither"
    TH.prefixWith "either" do
      TH.asIdent do
        TH.var "map"
        TH.var "bind"
        TH.var "pure"

  TH.mod "Erl.Data.List.Types" do
    TH.asIdent do
      TH.asString do
        TH.var "functorList"
        TH.var "functorWithIndexIntList"
        TH.var "functorNonEmptyList"
        TH.var "functorWithIndexIntNonEmp"

  -- TH.mod "List" do
  --   TH.prefixWith "list" do
  --     TH.asIdent do
  --       TH.var "map"
  --       TH.var "bind"
  --       TH.var "pure"

  -- TH.mod "Map" do
  --   TH.prefixWith "map" do
  --     TH.asIdent do
  --       TH.var "map"
  --       TH.var "bind"
  --       TH.var "pure"

  TH.mod "Maybe" do
    TH.asIdent do TH.asString do TH.var "functorMaybe"
    TH.prefixWith "maybe" do
      TH.asIdent do
        TH.var "map"
        TH.var "bind"
        TH.var "pure"

  TH.mod "Data.Array.NonEmpty.Internal" do
    TH.var "functorNonEmptyArray"

  TH.mod "NonEmptyArray" do
    TH.prefixWith "nonEmptyArray" do
      TH.asIdent do
        TH.var "map"
        TH.var "bind"
        TH.var "pure"

  TH.mod "NonEmptyList" do
    TH.prefixWith "nonEmptyList" do
      TH.asIdent do
        TH.var "map"
        TH.var "bind"
        TH.var "pure"

  -- TH.mod "Queue" do
  --   TH.prefixWith "queue" do
  --     TH.asIdent do
  --       TH.var "map"
  --       TH.var "bind"
  --       TH.var "pure"

  -- TH.mod "Set" do
  --   TH.prefixWith "set" do
  --     TH.asIdent do
  --       TH.var "map"
  --       TH.var "bind"
  --       TH.var "pure"

  -- TH.mod "String" do
  --   TH.prefixWith "string" do
  --     TH.asIdent do
  --       TH.var "map"
  --       TH.var "bind"
  --       TH.var "pure"

  -- TH.mod "Tuple" do
  --   TH.prefixWith "tuple" do
  --     TH.asIdent do
  --       TH.var "map"
  --       TH.var "bind"
  --       TH.var "pure"

  -- TH.mod "Variant" do
  --   TH.prefixWith "variant" do
  --     TH.asIdent do
  --       TH.var "map"
  --       TH.var "bind"
  --       TH.var "pure"

  TH.mod "Veither" do
    TH.prefixWith "veither" do
      TH.asIdent do
        TH.var "map"
        TH.var "bind"
        TH.var "pure"

  TH.mod "VexceptT" do
    TH.prefixWith "vexceptT" do
      TH.asIdent do
        TH.var "map"
        TH.var "bind"
        TH.var "pure"

  )
