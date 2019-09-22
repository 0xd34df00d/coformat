{-# LANGUAGE DataKinds, TypeFamilies #-}

module Clang.Format.Descr where

import qualified Data.Text as T
import Data.Void
import Numeric.Natural

data ConfigItemType = ParsedInfo | SupportedInfo | Instance

type family ConfigTypeData f ty where
  ConfigTypeData 'ParsedInfo _ = ()
  ConfigTypeData 'SupportedInfo Void = Void
  ConfigTypeData 'SupportedInfo _ = ()
  ConfigTypeData 'Instance ty = ty

data ConfigTypeT f
  = CTInt (ConfigTypeData f Int)
  | CTUnsigned (ConfigTypeData f Natural)
  | CTBool (ConfigTypeData f Bool)
  | CTString (ConfigTypeData f Void)
  | CTStringVec (ConfigTypeData f Void)
  | CTRawStringFormats (ConfigTypeData f Void)
  | CTIncludeCats (ConfigTypeData f Void)
  | CTEnum { variants :: [T.Text], enumValue :: ConfigTypeData f T.Text }

type ConfigType = ConfigTypeT 'ParsedInfo
type ConfigTypeSupported = ConfigTypeT 'ParsedInfo
type ConfigValue = ConfigTypeT 'Instance

data ConfigItemT f = ConfigItem
  { name :: T.Text
  , typ :: ConfigTypeT f
  }

type ConfigItem = ConfigItemT 'ParsedInfo
type ConfigItemSupported = ConfigItemT 'SupportedInfo
type ConfigInstance = ConfigItemT 'Instance
