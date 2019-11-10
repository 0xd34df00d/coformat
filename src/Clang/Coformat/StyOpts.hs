{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts, GADTs #-}
{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Clang.Coformat.StyOpts where

import qualified Data.Text as T
import Data.String.Interpolate.IsString
import Data.Void

import Clang.Format.Descr

data StyOpts = StyOpts
  { basedOnStyle :: T.Text
  , overriddenOpts :: [ConfigItemT 'Value]
  }

formatStyArg :: StyOpts -> T.Text
formatStyArg StyOpts { .. } = "{ " <> T.intercalate ", " opts <> " }"
  where
    opts = [i|BasedOnStyle: #{basedOnStyle}|]
         : [ [i|#{name}: #{fmtTyp typ}|]
           | ConfigItem { .. } <- overriddenOpts
           ]
    fmtTyp (CTInt n) = show n
    fmtTyp (CTUnsigned n) = show n
    fmtTyp (CTString v) = absurd v
    fmtTyp (CTStringVec v) = absurd v
    fmtTyp (CTRawStringFormats v) = absurd v
    fmtTyp (CTIncludeCats v) = absurd v
    fmtTyp (CTBool b) = if b then "true" else "false"
    fmtTyp (CTEnum _ opt) = T.unpack opt
