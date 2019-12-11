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
import Control.Monad.Except.CoHas
import Control.Monad.Extra
import Control.Monad.Logger
import Control.Monad.Reader.Has hiding(update)
import Control.Monad.State.Strict
import Data.Foldable
import Data.Maybe
import Data.Ord
import Data.Proxy
import Data.String.Interpolate.IsString
import System.Command.QQ

import Clang.Coformat.Score
import Clang.Coformat.StyOpts
import Clang.Coformat.Util
import Clang.Coformat.Variables
import Clang.Format.Descr
import Text.EditDistance

data OptEnv = OptEnv
  { baseStyle :: T.Text
  , files :: [String]
  , categoricalVariables :: [IxedCategoricalVariable]
  , integralVariables :: [IxedIntegralVariable]
  , constantOpts :: [ConfigItemT 'Value]
  }

runClangFormat :: (MonadError err m, CoHas UnexpectedFailure err, CoHas ExpectedFailure err, MonadIO m, MonadLogger m)
               => String -> String -> BSL.ByteString -> m Score
runClangFormat file logStr formattedSty = do
  let unpackedSty = BSL.unpack formattedSty
  stdout <- checked [sh|clang-format --style='#{unpackedSty}' #{file}|]
  source <- liftIO $ readFile file
  let dist = levenshteinDistance defaultEditCosts source $ TL.unpack stdout
  logDebugN [i|#{logStr}: #{dist}|]
  pure $ Score dist

type OptMonad err r m = (MonadLoggerIO m, MonadError err m, CoHas UnexpectedFailure err, MonadReader r m, Has OptEnv r)

runClangFormatFiles :: (OptMonad err r m, CoHas ExpectedFailure err)
                    => [ConfigItemT 'Value] -> String -> m Score
runClangFormatFiles varOpts logStr = do
  OptEnv { .. } <- ask
  let sty = StyOpts { basedOnStyle = baseStyle, additionalOpts = constantOpts <> varOpts }
  let formattedSty = formatStyArg sty
  fmap mconcat $ forM files $ \file -> runClangFormat file [i|#{logStr} at #{file}|] formattedSty

chooseBaseStyle :: (MonadError String m, MonadLoggerIO m) => [T.Text] -> [String] -> m (T.Text, Score)
chooseBaseStyle baseStyles files = do
  sty2dists <- forConcurrently' ((,) <$> baseStyles <*> files) $ \(sty, file) -> do
    let formattedArg = formatStyArg StyOpts { basedOnStyle = sty, additionalOpts = [] }
    convert (show @(Either ExpectedFailure UnexpectedFailure)) $ (sty,) <$> runClangFormat file [i|Initial guess for #{sty} at #{file}|] formattedArg
  let accumulated = HM.toList $ HM.fromListWith (<>) sty2dists
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

initOptState :: [ConfigItemT 'Value] -> Score -> OptState
initOptState currentOpts currentScore = OptState { .. }

dropExpectedFailures :: OptMonad err r m
                     => (forall err' r' m'. (OptMonad err' r' m', CoHas ExpectedFailure err') => m' Score)
                     -> m Score
dropExpectedFailures act = do
  res <- runExceptT act
  case res of
       Left (UnexpectedFailure failure) -> throwError failure
       Left (ExpectedFailure failure) -> do
         logErrorN [i|Unable to run the formatter: #{show failure}|]
         pure maxBound
       Right sc -> pure sc

type BestVals = [(ConfigTypeT 'Value, Score, Int)]

chooseBestVals :: (OptMonad err r m, Has OptState r, Has TaskGroup r)
               => [SomeIxedVariable] -> m BestVals
chooseBestVals ixedVariables = do
  env@OptEnv { .. } <- ask
  OptState { .. } <- ask
  partialResults <- forConcurrentlyPooled ixedVariables $ \(SomeIxedVariable (IxedVariable (MkDV (_ :: a)) idx)) -> flip runReaderT env $ do
    let optName = name $ currentOpts !! idx
    opt2scores <- forM (variateAt @a Proxy idx currentOpts) $ \opts' -> do
      let optValue = typ $ opts' !! idx
      sumScore <- dropExpectedFailures $ runClangFormatFiles opts' [i|Variate guess for #{optName}=#{optValue}|]
      logDebugN [i|Total dist for #{optName}=#{optValue}: #{sumScore}|]
      pure (optValue, sumScore)
    let (bestOptVal, bestScore) = minimumBy (comparing snd) opt2scores
    logDebugN [i|Best step for #{optName}: #{bestOptVal} at #{bestScore} (compare to #{currentScore})|]
    pure $ if bestScore < currentScore
            then Just (bestOptVal, bestScore, idx)
            else Nothing
  pure $ catMaybes partialResults

applyBestVals :: (OptMonad err r m, Has TaskGroup r, MonadState OptState m)
              => BestVals -> m ()
applyBestVals [] = logInfoN [i|Nothing got better on this iteration|]
applyBestVals results = do
  current <- get
  let curOpts = currentOpts current
  forM_ results $ \(val, score', idx) -> logInfoN [i|Setting #{name $ curOpts !! idx} to #{val} (#{currentScore current} -> #{score'})|]
  let nextOpts = foldr (\(val, _, idx) -> update idx (\cfg -> cfg { typ = val })) curOpts results
  sumScore <- dropExpectedFailures $ runClangFormatFiles nextOpts [i|Total score after optimization|]
  let (bestVal, bestScore, bestIdx) = minimumBy (comparing (^._2)) results
  if sumScore <= bestScore
    then do
      logInfoN [i|Total score after optimization on all files: #{sumScore}|]
      put OptState { currentOpts = nextOpts, currentScore = sumScore }
    else do
      logWarnN [i|Greedy algorithm failed (got #{sumScore} while best individual is #{bestScore})|]
      let nextOpts' = update bestIdx (\cfg -> cfg { typ = bestVal }) curOpts
      put OptState { currentOpts = nextOpts', currentScore = bestScore }

stepGDGeneric' :: (OptMonad err r m, Has TaskGroup r, MonadState OptState m)
               => [OptEnv -> [SomeIxedVariable]] -> m ()
stepGDGeneric' varGetters = do
  current <- get
  env@OptEnv { .. } <- ask
  tg <- ask
  results <- runReaderT (chooseBestVals $ concatMap ($ env) varGetters) (current, env, tg :: TaskGroup)
  applyBestVals results

stepGDGeneric :: (OptMonad err r m, Has TaskGroup r, MonadState OptState m)
              => [OptEnv -> [SomeIxedVariable]] -> m ()
stepGDGeneric varGetters = whenM ((> mempty) <$> gets currentScore) $ stepGDGeneric' varGetters

stepGD :: (OptMonad err r m, Has TaskGroup r, MonadState OptState m) => m ()
stepGD = stepGDGeneric [asSome . categoricalVariables, asSome . integralVariables]

fixGD :: (OptMonad err r m, Has TaskGroup r, MonadState OptState m, err ~ UnexpectedFailure) => Maybe Int -> m ()
fixGD (Just 0) = pure ()
fixGD counter = do
  startScore <- gets currentScore
  stepGD
  endScore <- gets currentScore
  logInfoN [i|Full optimization step done, went from #{startScore} to #{endScore}|]
  if startScore /= endScore
    then fixGD $ subtract 1 <$> counter
    else logInfoN [i|Done optimizing, stopped at score #{endScore}|]
