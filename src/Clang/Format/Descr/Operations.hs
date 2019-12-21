{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE RecordWildCards, LambdaCase #-}

module Clang.Format.Descr.Operations where

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Maybe

import Clang.Format.Descr

filterParsedItems :: [ConfigItemT 'Parsed] -> [ConfigItemT 'Supported]
filterParsedItems = mapMaybe $ \ConfigItem { .. } -> ConfigItem name <$> filterType typ
  where
    filterType = \case
                    CTInt () -> Just $ CTInt ()
                    CTUnsigned () -> Just $ CTUnsigned ()
                    CTBool () -> Just $ CTBool ()
                    CTString () -> Nothing
                    CTStringVec () -> Nothing
                    CTRawStringFormats () -> Nothing
                    CTIncludeCats () -> Nothing
                    CTEnum vars () -> Just $ CTEnum vars ()

replaceItemsWith :: [ConfigItemT 'Value] -> [ConfigItemT 'Value] -> [ConfigItemT 'Value]
replaceItemsWith l1 l2 = M.elems $ toMap l2 <> toMap l1
  where
    toMap lst = M.fromList [ (name item, item) | item <- lst ]

subtractMatching :: [ConfigItemT 'Value] -> [ConfigItemT 'Value] -> [ConfigItemT 'Value]
subtractMatching minuend subtrahend = filter f minuend
  where
    f ConfigItem { .. } = (/= Just typ) $ HM.lookup name subMap
    subMap = HM.fromList [ (name, typ) | ConfigItem { .. } <- subtrahend]
