{-# LANGUAGE DeriveAnyClass #-}
-- |
-- Source position information
--
module Language.PureScript.AST.SourcePos where

import Prelude

import Codec.Serialise (Serialise, encode, decode)
import Codec.Serialise.Encoding (encodeSimple)
import Codec.Serialise.Decoding (decodeSimple)
import Control.DeepSeq (NFData)
import Data.Aeson ((.=), (.:))
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.PureScript.Comments (Comment)
import Data.Aeson qualified as A
import Data.Text qualified as T
import System.FilePath (makeRelative)

-- | Source annotation - position information and comments.
data SourceAnn = SourceAnn !SourceSpan ![Comment]
  deriving (Show, Eq, Ord, Generic, NFData)

instance Serialise SourceAnn where
  encode sa =
    case sa of
      SourceAnn NullSourceSpan [] -> encodeSimple 0
      SourceAnn ss [] -> encodeSimple 1 <> encode ss
      SourceAnn NullSourceSpan comments -> encodeSimple 2 <> encode comments
      SourceAnn ss comments -> encodeSimple 3 <> encode ss <> encode comments

  decode = do
    tag <- decodeSimple
    case tag of
      0 ->
        pure $ SourceAnn NullSourceSpan []
      1 -> do
        ss <- decode
        pure $ SourceAnn ss []
      2 -> do
        comments <- decode
        pure $ SourceAnn NullSourceSpan comments
      3 -> do
        ss <- decode
        comments <- decode
        pure $ SourceAnn ss comments


safst (SourceAnn a _) = a
sasnd (SourceAnn _ b) = b

-- | Source position information
data SourcePos = SourcePos
  { sourcePosLine :: !Int
    -- ^ Line number
  , sourcePosColumn :: !Int
    -- ^ Column number
  } deriving (Show, Eq, Ord, Generic, NFData, Serialise)

displaySourcePos :: SourcePos -> Text
displaySourcePos sp =
  "line " <> T.pack (show (sourcePosLine sp)) <>
    ", column " <> T.pack (show (sourcePosColumn sp))

displaySourcePosShort :: SourcePos -> Text
displaySourcePosShort sp =
  T.pack (show (sourcePosLine sp)) <>
    ":" <> T.pack (show (sourcePosColumn sp))

instance A.ToJSON SourcePos where
  toJSON SourcePos{..} =
    A.toJSON [sourcePosLine, sourcePosColumn]

instance A.FromJSON SourcePos where
  parseJSON arr = do
    [line, col] <- A.parseJSON arr
    return $ SourcePos line col

data SourceSpan = SourceSpan
  { spanName :: !Text
    -- ^ Source name
  , spanStart :: !SourcePos
    -- ^ Start of the span
  , spanEnd :: !SourcePos
    -- ^ End of the span
  } deriving (Eq, Ord, Generic, NFData, Serialise)

instance Show SourceSpan where
  show NullSourceSpan = "s0"
  show _ = "ss"

displayStartEndPosShort :: SourceSpan -> Text
displayStartEndPosShort sp =
  displaySourcePosShort (spanStart sp) <> " - " <>
  displaySourcePosShort (spanEnd sp)

displaySourceSpan :: FilePath -> SourceSpan -> Text
displaySourceSpan relPath sp =
  T.pack (makeRelative relPath (T.unpack $ spanName sp)) <> ":" <>
    displayStartEndPosShort sp

instance A.ToJSON SourceSpan where
  toJSON SourceSpan{..} =
    A.object [ "name"  .= spanName
             , "start" .= spanStart
             , "end"   .= spanEnd
             ]

instance A.FromJSON SourceSpan where
  parseJSON = A.withObject "SourceSpan" $ \o ->
    SourceSpan     <$>
      o .: "name"  <*>
      o .: "start" <*>
      o .: "end"

instance A.ToJSON SourceAnn where
  toJSON (SourceAnn ss c) =
    A.object [ "ss"  .= ss
             , "comments" .= c
             ]

instance A.FromJSON SourceAnn where
  parseJSON = A.withObject "SourceAnn" $ \o ->
    SourceAnn     <$>
      o .: "ss"  <*>
      o .: "comments"

internalModuleSourceSpan :: T.Text -> SourceSpan
internalModuleSourceSpan name = SourceSpan name (SourcePos 0 0) (SourcePos 0 0)

nullSourceSpan :: SourceSpan
nullSourceSpan = internalModuleSourceSpan ""

nullSourceAnn :: SourceAnn
nullSourceAnn = SourceAnn nullSourceSpan []

pattern NullSourceSpan :: SourceSpan
pattern NullSourceSpan = SourceSpan "" (SourcePos 0 0) (SourcePos 0 0)

pattern NullSourceAnn :: SourceAnn
pattern NullSourceAnn = SourceAnn NullSourceSpan []

nonEmptySpan :: SourceAnn -> Maybe SourceSpan
nonEmptySpan (SourceAnn NullSourceSpan _) = Nothing
nonEmptySpan (SourceAnn ss _) = Just ss

widenSourceSpan :: SourceSpan -> SourceSpan -> SourceSpan
widenSourceSpan NullSourceSpan b = b
widenSourceSpan a NullSourceSpan = a
widenSourceSpan (SourceSpan n1 s1 e1) (SourceSpan n2 s2 e2) =
  SourceSpan n (min s1 s2) (max e1 e2)
  where
  n | n1 == ""  = n2
    | otherwise = n1

widenSourceAnn :: SourceAnn -> SourceAnn -> SourceAnn
widenSourceAnn (SourceAnn s1 _) (SourceAnn s2 _) = SourceAnn (widenSourceSpan s1 s2) []
