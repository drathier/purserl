-- @shouldFailWith OrphanInstance
module Main where

import Prelude

type Something = Int

derive instance eqSomething ∷ Eq Something
