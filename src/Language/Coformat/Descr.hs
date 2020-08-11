{-# LANGUAGE DataKinds, TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving, FlexibleInstances #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE Strict #-}

module Language.Coformat.Descr where

import qualified Data.Text as T
import Data.Void
import Numeric.Natural
import Language.Haskell.TH.Syntax

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
  | CTEnum { variants :: [T.Text], enumValue :: CTData f T.Text }
  | CTUnsupported (CTData f Void)

deriving instance Show (ConfigTypeT 'Parsed)
deriving instance Show (ConfigTypeT 'Supported)
deriving instance Show (ConfigTypeT 'Value)
deriving instance Lift (ConfigTypeT 'Supported)
deriving instance Eq (ConfigTypeT 'Value)

data ConfigItemT f = ConfigItem
  { name :: [T.Text]
  , value :: ConfigTypeT f
  }

deriving instance Show (ConfigItemT 'Parsed)
deriving instance Show (ConfigItemT 'Supported)
deriving instance Show (ConfigItemT 'Value)
deriving instance Lift (ConfigItemT 'Supported)

data OptsDescription stage = OptsDescription
  { knownOpts :: [ConfigItemT stage]
  , baseStyles :: [T.Text]
  }

deriving instance Lift (OptsDescription 'Supported)
