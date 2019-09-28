{-# LANGUAGE DataKinds, TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Clang.Format.Descr where

import qualified Data.Text as T
import Data.Maybe
import Data.Void
import Numeric.Natural

data Stage = Parsed | Supported | Value

type family CTData f ty where
  CTData 'Parsed _ = ()
  CTData 'Supported Void = Void
  CTData 'Supported _ = ()
  CTData 'Value ty = ty

data ConfigTypeT f
  = CTInt (CTData f Int)
  | CTUnsigned (CTData f Natural)
  | CTBool (CTData f Bool)
  | CTString (CTData f Void)
  | CTStringVec (CTData f Void)
  | CTRawStringFormats (CTData f Void)
  | CTIncludeCats (CTData f Void)
  | CTEnum { variants :: [T.Text], enumValue :: CTData f T.Text }

data ConfigItemT f = ConfigItem
  { name :: T.Text
  , typ :: ConfigTypeT f
  }

filterParsedItems :: [ConfigItemT 'Parsed] -> [ConfigItemT 'Supported]
filterParsedItems = mapMaybe $ \ConfigItem { .. } -> ConfigItem name <$> filterType typ
  where
    filterType typ = case typ of
                          CTInt () -> Just $ CTInt ()
                          CTUnsigned () -> Just $ CTUnsigned ()
                          CTBool () -> Just $ CTBool ()
                          CTString () -> Nothing
                          CTStringVec () -> Nothing
                          CTRawStringFormats () -> Nothing
                          CTIncludeCats () -> Nothing
                          CTEnum vars () -> Just $ CTEnum vars ()
