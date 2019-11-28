{-# LANGUAGE GADTs, TypeFamilies, DataKinds, TypeApplications, ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFoldable #-}

module Clang.Coformat.Variables where

import qualified Data.Text as T
import Control.Lens
import Control.Monad
import Numeric.Natural

import Clang.Format.Descr

data KnownVariateType = Categorical | Integral

class Variate a where
  type VariateResult a :: * -> *
  type VariateType a :: KnownVariateType
  variate :: a -> VariateResult a a
  varPrism :: Prism' (ConfigTypeT 'Value) a

data Variable varTy where
  -- TODO proxy should be enough
  MkDV :: (Variate a, VariateType a ~ varTy, Foldable (VariateResult a)) => a -> Variable varTy

data IxedVariable varTy = IxedVariable
  { discreteVar :: Variable varTy
  , varIdx :: Int
  }

type CategoricalVariate a = (Variate a, VariateType a ~ 'Categorical)
type CategoricalVariable = Variable 'Categorical
type IxedCategoricalVariable = IxedVariable 'Categorical


instance Variate Bool where
  type VariateResult Bool = []
  type VariateType Bool = 'Categorical
  variate b = [not b]
  varPrism = prism' CTBool $ \case CTBool b -> Just b
                                   _ -> Nothing

type EnumVar = ([T.Text], T.Text)

instance Variate EnumVar where
  type VariateResult EnumVar = []
  type VariateType EnumVar = 'Categorical
  variate (vars, cur) = [(vars, next) | next <- vars, next /= cur]
  varPrism = prism' (uncurry CTEnum) $ \case CTEnum vars cur -> Just (vars, cur)
                                             _ -> Nothing

typToDV :: ConfigTypeT 'Value -> Maybe CategoricalVariable
typToDV val = msum [ MkDV <$> val ^? varPrism @Bool
                   , MkDV <$> val ^? varPrism @([T.Text], T.Text)
                   ]


data IntegralVariateResult a
  = None
  | SingleDirection a
  | TwoDirections a a
  deriving (Eq, Show, Foldable)

instance Variate Int where
  type VariateResult Int = IntegralVariateResult
  type VariateType Int = 'Integral
  variate n = TwoDirections (n - 1) (n + 1)
  varPrism = prism' CTInt $ \case CTInt n -> Just n
                                  _ -> Nothing

instance Variate Natural where
  type VariateResult Natural = IntegralVariateResult
  type VariateType Natural = 'Integral
  variate n | n > 0 = TwoDirections (n - 1) (n + 1)
            | otherwise = SingleDirection (n + 1)
  varPrism = prism' CTUnsigned $ \case CTUnsigned n -> Just n
                                       _ -> Nothing

type IntegralVariate a = (Variate a, VariateType a ~ 'Integral)
type IntegralVariable = Variable 'Integral
type IxedIntegralVariable = IxedVariable 'Integral

typToIV :: ConfigTypeT 'Value -> Maybe IntegralVariable
typToIV val = msum [ MkDV <$> val ^? varPrism @Int
                   , MkDV <$> val ^? varPrism @Natural
                   ]
