{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts, GADTs #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Clang.Coformat.StyOpts where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens(_Object)
import Data.List
import Data.Maybe
import Data.Void

import Clang.Format.Descr

data StyOpts = StyOpts
  { basedOnStyle :: T.Text
  , additionalOpts :: [ConfigItemT 'Value]
  }

instance ToJSON StyOpts where
  toJSON StyOpts { .. } = Object $ foldl' f (HM.singleton "BasedOnStyle" $ String basedOnStyle) additionalOpts
    where
      f hm ConfigItem { .. } = updateRec typ name hm

      updateRec typ [key] = HM.insert key (toJson typ)
      updateRec typ (key:rest) = HM.alter (Just . Object . updateRec typ rest . getObj) key
      updateRec _ [] = id
      getObj = fromMaybe mempty . ((^? _Object) =<<)

      toJson (CTInt n) = Number $ fromIntegral n
      toJson (CTUnsigned n) = Number $ fromIntegral n
      toJson (CTString v) = absurd v
      toJson (CTStringVec v) = absurd v
      toJson (CTRawStringFormats v) = absurd v
      toJson (CTIncludeCats v) = absurd v
      toJson (CTBool b) = Bool b
      toJson (CTEnum _ opt) = String opt

formatStyArg :: StyOpts -> BSL.ByteString
formatStyArg = encode
