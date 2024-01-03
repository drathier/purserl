{-# LANGUAGE DeriveAnyClass #-}
module Language.PureScript.Erl.Errors.Types where

import Prelude.Compat

import Language.PureScript.Names
import Data.Text
import Language.PureScript.AST.Declarations (ErrorMessageHint(..))
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

-- | A type of error messages
data SimpleErrorMessage
  = FileIOError Text Text -- ^ A description of what we were trying to do, and the error which occurred
  | InvalidFFIArity ModuleName Text Int Int
  | MissingFFIModule ModuleName
  | UnnecessaryFFIModule ModuleName FilePath
  | MissingFFIImplementations ModuleName [Ident]
  | UnusedFFIImplementations ModuleName [Ident]
  | InternalError Text
  deriving (Show, Generic, NFData)


data ErrorMessage = ErrorMessage
  [ErrorMessageHint]
  SimpleErrorMessage
  deriving (Show, Generic, NFData)