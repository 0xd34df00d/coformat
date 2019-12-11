{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies, DerivingVia #-}

module Clang.Coformat.Score where

import Data.Monoid

newtype Score = Score { getScore :: Int } deriving (Eq, Ord, Show)
                                          deriving newtype (Bounded)
                                          deriving (Semigroup, Monoid) via (Sum Int)
