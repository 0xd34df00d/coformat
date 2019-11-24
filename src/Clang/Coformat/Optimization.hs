{-# LANGUAGE GADTs, DataKinds, TypeApplications, RankNTypes, ScopedTypeVariables, ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes, RecordWildCards, TupleSections #-}

module Clang.Coformat.Optimization where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Lens
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader.Has hiding(update)
import Control.Monad.State.Strict
import Data.Foldable
import Data.Maybe
import Data.Ord
import Data.Proxy
import Data.String.Interpolate.IsString
import System.Command.QQ

import Clang.Coformat.StyOpts
import Clang.Coformat.Util
import Clang.Coformat.Variables
import Clang.Format.Descr
import Text.Levenshteins

data OptEnv = OptEnv
  { baseStyle :: T.Text
  , files :: [String]
  , categoricalVariables :: [IxedCategoricalVariable]
  , integralVariables :: [IxedIntegralVariable]
  , constantOpts :: [ConfigItemT 'Value]
  }

newtype Score = Score { getScore :: Int } deriving (Eq, Ord, Show, Num)

runClangFormat :: (MonadError String m, MonadIO m, MonadLogger m)
               => StringNormalizer -> String -> String -> BSL.ByteString -> m Score
runClangFormat norm file logStr formattedSty = do
  let unpackedSty = BSL.unpack formattedSty
  stdout <- checked [sh|clang-format --style='#{unpackedSty}' #{file}|]
  source <- liftIO $ readFile file
  let dist = levenshteinDistanceWith norm source $ TL.unpack stdout
  logDebugN [i|#{logStr}: #{dist}|]
  pure $ Score dist

type OptMonad r m = (MonadLoggerIO m, MonadError String m, MonadReader r m, Has OptEnv r)

runClangFormatFiles :: OptMonad r m
                    => StringNormalizer -> [ConfigItemT 'Value] -> String -> m Score
runClangFormatFiles norm varOpts logStr = do
  OptEnv { .. } <- ask
  let sty = StyOpts { basedOnStyle = baseStyle, additionalOpts = constantOpts <> varOpts }
  let formattedSty = formatStyArg sty
  fmap sum $ forM files $ \file -> runClangFormat norm file [i|#{logStr} at #{file}|] formattedSty

chooseBaseStyle :: (MonadError String m, MonadLoggerIO m) => [T.Text] -> [String] -> m (T.Text, Score)
chooseBaseStyle baseStyles files = do
  sty2dists <- forConcurrently' ((,) <$> baseStyles <*> files) $ \(sty, file) ->
    (sty,) <$> runClangFormat initialOptNormalizer file [i|Initial guess for #{sty} at #{file}|] (formatStyArg StyOpts { basedOnStyle = sty, additionalOpts = [] })
  let accumulated = HM.toList $ HM.fromListWith (+) sty2dists
  forM_ accumulated $ \(sty, acc) -> logInfoN [i|Initial accumulated guess for #{sty}: #{acc}|]
  pure $ minimumBy (comparing snd) accumulated

variateAt :: forall a. (Variate a, Foldable (VariateResult a))
          => Proxy a -> Int -> [ConfigItemT 'Value] -> [[ConfigItemT 'Value]]
variateAt _ idx opts = [ update idx (updater v') opts | v' <- toList variated ]
  where
    thePrism :: Prism' (ConfigTypeT 'Value) a
    thePrism = varPrism
    variated = variate $ typ (opts !! idx) ^?! thePrism
    updater v cfg = cfg { typ = typ cfg & thePrism .~ v }

data OptState = OptState
  { currentOpts :: [ConfigItemT 'Value]
  , currentScore :: Score
  } deriving (Show)

chooseBestVals :: (OptMonad r m, Has OptState r, Foldable varTy)
               => [IxedVariable varTy] -> m [(ConfigTypeT 'Value, Score, Int)]
chooseBestVals ixedVariables = do
  env@OptEnv { .. } <- ask
  OptState { .. } <- ask
  partialResults <- forConcurrently' ixedVariables $ \(IxedVariable (MkDV (_ :: a)) idx) -> flip runReaderT env $ do
    let optName = name $ currentOpts !! idx
    opt2scores <- forM (variateAt @a Proxy idx currentOpts) $ \opts' -> do
      let optValue = typ $ opts' !! idx
      sumScore <- runClangFormatFiles initialOptNormalizer opts' [i|Variate guess for #{optName}=#{optValue}|]
      logDebugN [i|Total dist for #{optName}=#{optValue}: #{sumScore}|]
      pure (optValue, sumScore)
    let (bestOptVal, bestScore) = minimumBy (comparing snd) opt2scores
    logDebugN [i|Best step for #{optName}: #{bestOptVal} at #{bestScore} (compare to #{currentScore})|]
    pure $ if bestScore < currentScore
            then Just (bestOptVal, bestScore, idx)
            else Nothing
  pure $ catMaybes partialResults

stepGDGeneric :: (OptMonad r m, MonadState OptState m, Foldable varTy)
              => StringNormalizer -> (OptEnv -> [IxedVariable varTy]) -> m ()
stepGDGeneric normalizer varGetter = do
  current <- get
  when (currentScore current > 0) $ do
    env@OptEnv { .. } <- ask
    results <- runReaderT (chooseBestVals $ varGetter env) (current, env)
    let nextOpts = foldr (\(val, _, idx) -> update idx (\cfg -> cfg { typ = val })) (currentOpts current) results
    sumScore <- runClangFormatFiles normalizer nextOpts [i|Total score after optimization|]
    logInfoN [i|Total score after optimization on all files: #{sumScore}|]
    put OptState { currentOpts = nextOpts, currentScore = sumScore }

initialOptNormalizer :: StringNormalizer
initialOptNormalizer = blindTokens . dropStartSpaces

stepGDCategorical :: (OptMonad r m, MonadState OptState m) => m ()
stepGDCategorical = stepGDGeneric initialOptNormalizer categoricalVariables

stepGDNumericMid :: (OptMonad r m, MonadState OptState m) => m ()
stepGDNumericMid = stepGDGeneric initialOptNormalizer integralVariables

stepGDNumericStart :: (OptMonad r m, MonadState OptState m) => m ()
stepGDNumericStart = stepGDGeneric leaveStartSpaces integralVariables

stepGD :: (OptMonad r m, MonadState OptState m) => m ()
stepGD = do
  stepGDCategorical
  stepGDNumericMid
  stepGDNumericStart
