module Language.PureScript.AST.Declarations.ChainId
  ( ChainId
  , mkChainId
  ) where

import Prelude
import Language.PureScript.AST.SourcePos qualified as Pos
import Control.DeepSeq (NFData)
import Codec.Serialise (Serialise)
import Data.Text qualified as T
import GHC.Generics (Generic)

-- |
-- For a given instance chain, stores the chain's file name and
-- the starting source pos of the first instance in the chain.
-- This data is used to determine which instances are part of
-- the same instance chain.
data ChainId = ChainId {-# UNPACK #-} !T.Text {-# UNPACK #-} !Pos.SourcePos
  deriving (Eq, Ord, Show, Generic)

instance NFData ChainId
instance Serialise ChainId

mkChainId :: T.Text -> Pos.SourcePos -> ChainId
mkChainId fileName startingSourcePos = ChainId fileName startingSourcePos
