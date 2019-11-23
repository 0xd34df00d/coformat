{-# LANGUAGE GADTs, DataKinds, TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Clang.Coformat.Variables where

import qualified Data.Text as T
import Control.Lens
import Control.Monad

import Clang.Format.Descr

class DiscreteVariate a where
  variate :: a -> [a]
  varPrism :: Prism' (ConfigTypeT 'Value) a

instance DiscreteVariate Bool where
  variate b = [not b]
  varPrism = prism' CTBool $ \case CTBool b -> Just b
                                   _ -> Nothing

instance DiscreteVariate ([T.Text], T.Text) where
  variate (vars, cur) = [(vars, next) | next <- vars, next /= cur]
  varPrism = prism' (uncurry CTEnum) $ \case CTEnum vars cur -> Just (vars, cur)
                                             _ -> Nothing

data DiscreteVariable where
  -- TODO proxy should be enough
  MkDV :: DiscreteVariate a => a -> DiscreteVariable

typToDV :: ConfigTypeT 'Value -> Maybe DiscreteVariable
typToDV val = msum [ MkDV <$> val ^? varPrism @Bool
                   , MkDV <$> val ^? varPrism @([T.Text], T.Text)
                   ]

data IxedDiscreteVariable = IxedDiscreteVariable
  { discreteVar :: DiscreteVariable
  , varIdx :: Int
  }
