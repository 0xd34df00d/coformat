{-# LANGUAGE DeriveGeneric, DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}

module Clang.Coformat.Score
( Score
, calcScore
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import Data.Char
import Data.Monoid
import GHC.Generics
import Generic.Data

data Score = Score
  { significantLettersCountsDiff :: Sum Int
  , spacesMisalignment :: Sum Int
  } deriving (Eq, Ord, Show, Bounded, Generic)
    deriving (Semigroup, Monoid) via (Generically Score)

calcScore :: BS.ByteString -> BS.ByteString -> Score
calcScore s1 s2 = Score { .. }
  where
    significantLettersCountsDiff = Sum $ calcLettersDiff s1 s2
    spacesMisalignment | significantLettersCountsDiff /= 0 = 0
                       | otherwise = Sum $ alignSpaces s1 s2

calcLettersDiff :: BS.ByteString -> BS.ByteString -> Int
calcLettersDiff s1 s2 = sum $ HM.elems $ HM.unionWith (\v1 v2 -> abs $ v1 - v2) s1counters s2counters
  where
    calcLetters = BS.foldl' ins mempty
    ins hm ch | isSpace ch = hm
              | otherwise = HM.insertWith (+) ch 1 hm
    s1counters = calcLetters s1
    s2counters = calcLetters s2

alignSpaces :: BS.ByteString -> BS.ByteString -> Int
alignSpaces bs1 bs2 = go (BS.unpack bs1) (BS.unpack bs2)
  where
    go s1 [] = length s1
    go [] s2 = length s2
    go (c1:s1) (c2:s2) | c1 == c2 = go s1 s2
                       | isSpace c1 && isSpace c2 = 2 + go s1 s2
                       | isSpace c1 = 1 + go s1 (c2:s2)
                       | isSpace c2 = 1 + go (c1:s1) s2
                       | otherwise = 2 + length s1 + length s2
