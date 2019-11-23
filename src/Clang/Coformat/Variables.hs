{-# LANGUAGE GADTs, TypeFamilies, DataKinds, TypeApplications, ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Clang.Coformat.Variables where

import qualified Data.Text as T
import Control.Lens
import Control.Monad

import Clang.Format.Descr

class Variate a where
  type VariateResult a :: * -> *
  variate :: a -> VariateResult a a
  varPrism :: Prism' (ConfigTypeT 'Value) a

instance Variate Bool where
  type VariateResult Bool = []
  variate b = [not b]
  varPrism = prism' CTBool $ \case CTBool b -> Just b
                                   _ -> Nothing

instance Variate ([T.Text], T.Text) where
  type VariateResult ([T.Text], T.Text) = []
  variate (vars, cur) = [(vars, next) | next <- vars, next /= cur]
  varPrism = prism' (uncurry CTEnum) $ \case CTEnum vars cur -> Just (vars, cur)
                                             _ -> Nothing

type CategoricalVariate a = (Variate a, VariateResult a ~ [])

data CategoricalVariable where
  -- TODO proxy should be enough
  MkDV :: CategoricalVariate a => a -> CategoricalVariable

typToDV :: ConfigTypeT 'Value -> Maybe CategoricalVariable
typToDV val = msum [ MkDV <$> val ^? varPrism @Bool
                   , MkDV <$> val ^? varPrism @([T.Text], T.Text)
                   ]

data IxedCategoricalVariable = IxedCategoricalVariable
  { discreteVar :: CategoricalVariable
  , varIdx :: Int
  }
