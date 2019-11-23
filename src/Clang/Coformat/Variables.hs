{-# LANGUAGE GADTs, TypeFamilies, DataKinds, TypeApplications, ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFoldable #-}

module Clang.Coformat.Variables where

import qualified Data.Text as T
import Control.Lens
import Control.Monad
import Numeric.Natural

import Clang.Format.Descr

class Variate a where
  type VariateResult a :: * -> *
  variate :: a -> VariateResult a a
  varPrism :: Prism' (ConfigTypeT 'Value) a

data Variable varTy where
  -- TODO proxy should be enough
  MkDV :: (Variate a, VariateResult a ~ varTy) => a -> Variable varTy

data IxedVariable varTy = IxedVariable
  { discreteVar :: Variable varTy
  , varIdx :: Int
  }

type CategoricalVariate a = (Variate a, VariateResult a ~ [])
type CategoricalVariable = Variable []
type IxedCategoricalVariable = IxedVariable []


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
  variate n = TwoDirections (n - 1) (n + 1)
  varPrism = prism' CTInt $ \case CTInt n -> Just n
                                  _ -> Nothing

instance Variate Natural where
  type VariateResult Natural = IntegralVariateResult
  variate n | n > 0 = TwoDirections (n - 1) (n + 1)
            | otherwise = SingleDirection (n + 1)
  varPrism = prism' CTUnsigned $ \case CTUnsigned n -> Just n
                                       _ -> Nothing

type IntegralVariate a = (Variate a, VariateResult a ~ IntegralVariateResult)
type IntegralVariable = Variable IntegralVariateResult
type IxedIntegralVariable = IxedVariable IntegralVariateResult

typToIV :: ConfigTypeT 'Value -> Maybe IntegralVariable
typToIV val = msum [ MkDV <$> val ^? varPrism @Int
                   , MkDV <$> val ^? varPrism @Natural
                   ]
