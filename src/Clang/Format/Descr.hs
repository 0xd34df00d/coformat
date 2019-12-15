{-# LANGUAGE DataKinds, TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving, FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

module Clang.Format.Descr where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
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

deriving instance Show (ConfigTypeT 'Parsed)
deriving instance Show (ConfigTypeT 'Supported)
deriving instance Show (ConfigTypeT 'Value)

data ConfigItemT f = ConfigItem
  { name :: [T.Text]
  , typ :: ConfigTypeT f
  }

deriving instance Show (ConfigItemT 'Parsed)
deriving instance Show (ConfigItemT 'Supported)
deriving instance Show (ConfigItemT 'Value)

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

replaceItemsWith :: [ConfigItemT 'Value] -> [ConfigItemT 'Value] -> [ConfigItemT 'Value]
replaceItemsWith l1 l2 = M.elems $ toMap l2 <> toMap l1
  where
    toMap lst = M.fromList [ (name item, item) | item <- lst ]
