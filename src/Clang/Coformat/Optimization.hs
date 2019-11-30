{-# LANGUAGE GADTs, DataKinds, TypeApplications, RankNTypes, ScopedTypeVariables, ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes, RecordWildCards, TupleSections #-}

module Clang.Coformat.Optimization where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Concurrent.Async.Pool
import Control.Lens
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.Logger
import Control.Monad.Reader.Has hiding(update)
import Control.Monad.State.Strict
import Data.Foldable
import Data.List.Extra
import Data.Maybe
import Data.Ord
import Data.Proxy
import Data.String.Interpolate.IsString
import GHC.Generics
import System.Command.QQ

import Clang.Coformat.Score
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
    (sty,) <$> runClangFormat initialNormalizer file [i|Initial guess for #{sty} at #{file}|] (formatStyArg StyOpts { basedOnStyle = sty, additionalOpts = [] })
  let accumulated = HM.toList $ HM.fromListWith (+) sty2dists
  forM_ accumulated $ \(sty, acc) -> logInfoN [i|Initial accumulated guess for #{sty}: #{acc}|]
  pure $ minimumBy (comparing snd) accumulated
  where
    initialNormalizer = score2norm ScoreMid

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
  , currentScores :: ScoresMap
  } deriving (Show, Generic, Has ScoresMap)

initOptState :: [ConfigItemT 'Value] -> Score -> OptState
initOptState currentOpts baseStyleScore = OptState { .. }
  where currentScores = HM.singleton ScoreMid baseStyleScore

fillAllScores :: (OptMonad r m, MonadState OptState m) => m ()
fillAllScores = do
  opts <- gets currentOpts
  presentTypes <- gets $ HM.keys . currentScores
  let filler scoreType | scoreType `elem` presentTypes = pure ()
                       | otherwise = do
                          sumScore <- runClangFormatFiles (score2norm scoreType) opts [i| |]
                          modify' $ \st -> st { currentScores = HM.insert scoreType sumScore $ currentScores st }
  mapM_ filler enumerate

type BestVals = [(ConfigTypeT 'Value, Score, Int)]

chooseBestVals :: (OptMonad r m, Has OptState r, Has TaskGroup r)
               => ScoreType -> [IxedVariable varTy] -> m BestVals
chooseBestVals scoreType ixedVariables = do
  env@OptEnv { .. } <- ask
  st@OptState { .. } <- ask
  let baseScore = score scoreType st
  partialResults <- forConcurrentlyPooled ixedVariables $ \(IxedVariable (MkDV (_ :: a)) idx) -> flip runReaderT env $ do
    let optName = name $ currentOpts !! idx
    opt2scores <- forM (variateAt @a Proxy idx currentOpts) $ \opts' -> do
      let optValue = typ $ opts' !! idx
      sumScore <- runClangFormatFiles (score2norm scoreType) opts' [i|Variate guess for #{optName}=#{optValue}|]
      logDebugN [i|Total dist for #{optName}=#{optValue}: #{sumScore}|]
      pure (optValue, sumScore)
    let (bestOptVal, bestScore) = minimumBy (comparing snd) opt2scores
    logDebugN [i|Best step for #{optName}: #{bestOptVal} at #{bestScore} (compare to #{baseScore})|]
    pure $ if bestScore < baseScore
            then Just (bestOptVal, bestScore, idx)
            else Nothing
  pure $ catMaybes partialResults

applyBestVals :: (OptMonad r m, Has TaskGroup r, MonadState OptState m)
              => ScoreType -> BestVals -> m ()
applyBestVals _ [] = logInfoN [i|Nothing got better on this iteration|]
applyBestVals scoreType results = do
  current <- get
  let curOpts = currentOpts current
  forM_ results $ \(val, score', idx) -> logInfoN [i|Setting #{name $ curOpts !! idx} to #{val} (#{score scoreType current} -> #{score'})|]
  let nextOpts = foldr (\(val, _, idx) -> update idx (\cfg -> cfg { typ = val })) curOpts results
  sumScore <- runClangFormatFiles normalizer nextOpts [i|Total score after optimization|]
  let (bestVal, bestScore, bestIdx)  = minimumBy (comparing (^._2)) results
  if sumScore <= bestScore
    then do
      logInfoN [i|Total score after optimization on all files: #{sumScore}|]
      put OptState { currentOpts = nextOpts, currentScores = HM.insert scoreType sumScore $ currentScores current }
    else do
      logWarnN [i|Greedy algorithm failed (got #{sumScore} while best individual is #{bestScore})|]
      let nextOpts' = update bestIdx (\cfg -> cfg { typ = bestVal }) curOpts
      put OptState { currentOpts = nextOpts', currentScores = HM.insert scoreType bestScore $ currentScores current }
  where
    normalizer = score2norm scoreType

stepGDGeneric' :: (OptMonad r m, Has TaskGroup r, MonadState OptState m)
               => (OptEnv -> [IxedVariable varTy]) -> ScoreType -> m ()
stepGDGeneric' varGetter scoreType = do
  current <- get
  env@OptEnv { .. } <- ask
  tg <- ask
  results <- runReaderT (chooseBestVals scoreType $ varGetter env) (current, env, tg :: TaskGroup)
  applyBestVals scoreType results

stepGDGeneric :: (OptMonad r m, Has TaskGroup r, MonadState OptState m)
              => (OptEnv -> [IxedVariable varTy]) -> ScoreType -> m ()
stepGDGeneric varGetter scoreType = whenM ((> 0) <$> scoreM scoreType) $ stepGDGeneric' varGetter scoreType

stepGDCategorical :: (OptMonad r m, Has TaskGroup r, MonadState OptState m) => m ()
stepGDCategorical = stepGDGeneric categoricalVariables ScoreMid

stepGDNumericMid :: (OptMonad r m, Has TaskGroup r, MonadState OptState m) => m ()
stepGDNumericMid = stepGDGeneric integralVariables ScoreMid

stepGDNumericStart :: (OptMonad r m, Has TaskGroup r, MonadState OptState m) => m ()
stepGDNumericStart = stepGDGeneric integralVariables ScoreStart

stepGD :: (OptMonad r m, Has TaskGroup r, MonadState OptState m) => m ()
stepGD = do
  startScore <- scoreM ScoreMid
  stepGDCategorical
  stepGDNumericMid
  midScore <- scoreM ScoreMid
  when (startScore == midScore) stepGDNumericStart

fixGD :: (OptMonad r m, Has TaskGroup r, MonadState OptState m) => Maybe Int -> m ()
fixGD (Just 0) = pure ()
fixGD counter = do
  fillAllScores
  startScore <- gets currentScores
  stepGD
  endScore <- gets currentScores
  logInfoN [i|Full optimization step done, went from #{startScore} to #{endScore}|]
  if startScore /= endScore
    then fixGD $ subtract 1 <$> counter
    else logInfoN [i|Done optimizing, stopped at score #{endScore}|]
