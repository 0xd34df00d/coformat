{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies #-}

module Clang.Coformat.Score where

newtype Score = Score { getScore :: Int } deriving (Eq, Ord, Show)
                                          deriving newtype (Num, Bounded)
