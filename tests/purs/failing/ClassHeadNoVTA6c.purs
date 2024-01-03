-- @shouldFailWith NoInstanceFound
module Main where

class MultiCoveringSets a b c d e f | a b -> c d e f, f e -> a b c d where
  noneOfSets :: Int

  partialOfABSet :: a -> { c :: c, d :: d }

  partialOfFESet :: f -> { c :: c, d :: d }

partialOfFESet'
  :: forall a b c d e f
   . MultiCoveringSets a b c d e f
  => f
  -> { c :: c, d :: d }
partialOfFESet' = partialOfFESet
