{-# LANGUAGE DeriveGeneric, DerivingVia #-}
{-# LANGUAGE RecordWildCards, QuasiQuotes #-}

module Language.Coformat.Score
( Score
, calcScore

, PreparedFile
, filename
, prepareFile
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap.Strict as IM
import Control.Monad.IO.Class
import Data.Char
import Data.Monoid
import Data.String.Interpolate
import GHC.Generics
import Generic.Data

type CharsHist = IM.IntMap Int

data PreparedFile = PreparedFile
  { filename :: FilePath
  , contents :: BS.ByteString
  , charsHist :: CharsHist
  } deriving (Eq)

prepareFile :: MonadIO m => FilePath -> m PreparedFile
prepareFile filename = do
  contents <- liftIO $ BS.readFile filename
  let charsHist = calcCharsHist contents
  pure $ PreparedFile { .. }

data Score = Score
  { significantLettersCountsDiff :: Sum Int
  , spacesMisalignment :: Sum Int
  } deriving (Eq, Ord, Bounded, Generic)
    deriving (Semigroup, Monoid) via (Generically Score)

instance Show Score where
  show Score { .. } = [i|Score { #{getSum significantLettersCountsDiff} / #{getSum spacesMisalignment} }|]

calcScore :: PreparedFile -> BS.ByteString -> Score
calcScore prepared str = Score { .. }
  where
    significantLettersCountsDiff = Sum $ calcCharsDiff (charsHist prepared) (calcCharsHist str)
    spacesMisalignment | significantLettersCountsDiff /= 0 = 0
                       | otherwise = Sum $ alignSpaces (contents prepared) str

calcCharsDiff :: CharsHist -> CharsHist -> Int
calcCharsDiff hm1 hm2 = sum $ IM.elems $ IM.unionWith (\v1 v2 -> abs $ v1 - v2) hm1 hm2

calcCharsHist :: BS.ByteString -> CharsHist
calcCharsHist = BS.foldl' ins mempty
  where
    ins hm ch | isSpace ch = hm
              | otherwise = IM.insertWith (+) (ord ch) 1 hm

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
