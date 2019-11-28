{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies, DeriveAnyClass, DeriveGeneric #-}

module Clang.Coformat.Score where

import qualified Data.HashMap.Strict as HM
import Control.Monad.Reader.Has
import Control.Monad.State.Strict
import Data.Hashable
import GHC.Generics

import Text.Levenshteins

newtype Score = Score { getScore :: Int } deriving (Eq, Ord, Show)
                                          deriving newtype Num

data ScoreType = ScoreMid | ScoreStart deriving (Eq, Ord, Show, Generic, Hashable, Enum, Bounded)

type ScoresMap = HM.HashMap ScoreType Score

score2norm :: ScoreType -> StringNormalizer
score2norm ScoreMid = blindTokens . dropStartSpaces
score2norm ScoreStart = leaveStartSpaces

score :: Has ScoresMap t => ScoreType -> t -> Score
score ty = (HM.! ty) . extract

scoreM :: (MonadState s m, Has ScoresMap s) => ScoreType -> m Score
scoreM ty = score ty <$> get
