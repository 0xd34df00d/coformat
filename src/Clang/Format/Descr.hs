{-# LANGUAGE DataKinds, TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving, FlexibleInstances #-}
{-# LANGUAGE Strict #-}

module Clang.Format.Descr where

import qualified Data.Text as T
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
deriving instance Eq (ConfigTypeT 'Value)

data ConfigItemT f = ConfigItem
  { name :: [T.Text]
  , value :: ConfigTypeT f
  }

deriving instance Show (ConfigItemT 'Parsed)
deriving instance Show (ConfigItemT 'Supported)
deriving instance Show (ConfigItemT 'Value)
