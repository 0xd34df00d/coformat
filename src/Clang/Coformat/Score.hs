{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies, DerivingVia #-}

module Clang.Coformat.Score where

import qualified Data.ByteString.Char8 as BS
import Data.Monoid

import Text.Levenshteins

newtype Score = Score { getScore :: Int } deriving (Eq, Ord, Show)
                                          deriving newtype (Bounded)
                                          deriving (Semigroup, Monoid) via (Sum Int)

calcScore :: BS.ByteString -> BS.ByteString -> Score
calcScore s1 s2 = Score $ levenshteinDistanceWith id (BS.unpack s1) (BS.unpack s2)
