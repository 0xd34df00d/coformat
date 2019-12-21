{-# LANGUAGE GADTs, DataKinds, TypeApplications, RankNTypes, ScopedTypeVariables, ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes, RecordWildCards, TupleSections, LambdaCase #-}

module Clang.Coformat.Optimization where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TL
import Control.Concurrent.Async.Pool
import Control.Lens
import Control.Monad.Except.CoHas
import Control.Monad.Extra
import Control.Monad.Logger
import Control.Monad.Reader.Has hiding(update)
import Control.Monad.State.Strict
import Data.Foldable
import Data.List
import Data.Ord
import Data.Proxy
import Data.String.Interpolate.IsString
import Numeric.Natural
import System.Command.QQ

import Clang.Coformat.Score
import Clang.Coformat.StyOpts
import Clang.Coformat.Util
import Clang.Coformat.Variables
import Clang.Format.Descr

data FmtEnv = FmtEnv
  { baseStyle :: T.Text
  , preparedFiles :: [PreparedFile]
  , constantOpts :: [ConfigItemT 'Value]
  }

data OptEnv = OptEnv
  { categoricalVariables :: [IxedCategoricalVariable]
  , integralVariables :: [IxedIntegralVariable]
  , maxSubsetSize :: Natural
  }

runClangFormat :: (MonadError err m, CoHas UnexpectedFailure err, CoHas ExpectedFailure err, MonadIO m, MonadLogger m)
               => PreparedFile -> String -> BSL.ByteString -> m Score
runClangFormat prepared logStr formattedSty = do
  stdout <- checked [sh|clang-format --style='#{unpackedSty}' #{path}|]
  let dist = calcScore prepared $ BSL.toStrict $ TL.encodeUtf8 stdout
  logDebugN [i|#{logStr}: #{dist}|]
  pure dist
  where
    path = filename prepared
    unpackedSty = BSL.unpack formattedSty

type OptMonad err r m = (MonadLoggerIO m, MonadError err m, CoHas UnexpectedFailure err, MonadReader r m, Has FmtEnv r)

runClangFormatFiles :: (OptMonad err r m, CoHas ExpectedFailure err)
                    => [ConfigItemT 'Value] -> String -> m Score
runClangFormatFiles varOpts logStr = do
  FmtEnv { .. } <- ask
  let sty = StyOpts { basedOnStyle = baseStyle, additionalOpts = constantOpts <> varOpts }
  let formattedSty = formatStyArg sty
  fmap mconcat $ forM preparedFiles $ \prepared -> runClangFormat prepared [i|#{logStr} at #{filename prepared}|] formattedSty

chooseBaseStyle :: (MonadError String m, MonadLoggerIO m) => [T.Text] -> [ConfigItemT 'Value] -> [PreparedFile] -> m (T.Text, Score)
chooseBaseStyle baseStyles predefinedOpts files = do
  sty2dists <- forConcurrently' ((,) <$> baseStyles <*> files) $ \(sty, file) -> do
    let formattedArg = formatStyArg StyOpts { basedOnStyle = sty, additionalOpts = predefinedOpts }
    convert (show @(Either ExpectedFailure UnexpectedFailure)) $ (sty,) <$> runClangFormat file [i|Initial guess for #{sty} at #{filename file}|] formattedArg
  let accumulated = HM.toList $ HM.fromListWith (<>) sty2dists
  forM_ accumulated $ \(sty, acc) -> logInfoN [i|Initial accumulated guess for #{sty}: #{acc}|]
  pure $ minimumBy (comparing snd) accumulated

variateAt :: forall a. (Variate a, Foldable (VariateResult a))
          => Proxy a -> Int -> [ConfigItemT 'Value] -> [[ConfigItemT 'Value]]
variateAt _ idx opts = [ update idx (updater v') opts | v' <- toList variated ]
  where
    thePrism :: Prism' (ConfigTypeT 'Value) a
    thePrism = varPrism
    variated = variate $ value (opts !! idx) ^?! thePrism
    updater v cfg = cfg { value = value cfg & thePrism .~ v }

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

variateSubset :: [SomeIxedVariable] -> [ConfigItemT 'Value] -> [[ConfigItemT 'Value]]
variateSubset [] opts = [opts]
variateSubset (SomeIxedVariable (IxedVariable (MkDV (_ :: a)) idx) : rest) opts = concatMap (variateSubset rest) $ variateAt @a Proxy idx opts

showVariated :: [SomeIxedVariable] -> [ConfigItemT 'Value] -> String
showVariated vars opts = intercalate ", " [showVar var | var <- vars]
  where
    showVar (SomeIxedVariable (IxedVariable _ idx)) = [i|#{name $ opts !! idx} -> #{value $ opts !! idx}|]

chooseBestSubset :: (OptMonad err r m, Has OptState r, Has TaskGroup r)
                 => Natural -> [SomeIxedVariable] -> m (Maybe ([ConfigItemT 'Value], Score))
chooseBestSubset subsetSize ixedVariables = do
  OptState { .. } <- ask
  partialResults <- forConcurrentlyPooled (subsetsN subsetSize ixedVariables) $ \someVarsSubset -> do
    opt2scores <- forM (variateSubset someVarsSubset currentOpts) $ \opts' ->
      fmap (opts',) $ dropExpectedFailures $ runClangFormatFiles opts' $ showVariated someVarsSubset opts'
    let (bestOpts, bestScore) = minimumBy (comparing snd) opt2scores
    when (bestScore < currentScore) $
      logInfoN [i|Total dist for #{showVariated someVarsSubset bestOpts}: #{currentScore} -> #{bestScore}|]
    pure (bestOpts, bestScore)
  let (bestOpts, bestScore) = minimumBy (comparing snd) partialResults
  pure $ if bestScore < currentScore
          then Just (bestOpts, bestScore)
          else Nothing

stepGDGeneric' :: (OptMonad err r m, Has TaskGroup r, Has OptEnv r, MonadState OptState m)
               => Natural -> [OptEnv -> [SomeIxedVariable]] -> m ()
stepGDGeneric' subsetSize varGetters = do
  current <- get
  fmtEnv@FmtEnv { .. } <- ask
  optEnv <- ask
  tg :: TaskGroup <- ask
  runReaderT (chooseBestSubset subsetSize $ concatMap ($ optEnv) varGetters) (current, fmtEnv, tg) >>=
    \case Nothing -> pure ()
          Just (opts', score') -> do
            logInfoN [i|Total score after optimization on all files: #{score'}|]
            put OptState { currentOpts = opts', currentScore = score' }

stepGDGeneric :: (OptMonad err r m, Has TaskGroup r, Has OptEnv r, MonadState OptState m)
              => Natural -> [OptEnv -> [SomeIxedVariable]] -> m ()
stepGDGeneric subsetSize varGetters = whenM ((> mempty) <$> gets currentScore) $ stepGDGeneric' subsetSize varGetters

fixGD :: (OptMonad err r m, Has TaskGroup r, Has OptEnv r, MonadState OptState m, err ~ UnexpectedFailure)
      => Maybe Int -> Natural -> m ()
fixGD (Just 0) _ = pure ()
fixGD counter curSubsetSize = do
  maxSubsetSize' <- asks maxSubsetSize
  if curSubsetSize > maxSubsetSize'
    then logInfoN [i|Done optimizing|]
    else do
      startScore <- gets currentScore
      stepGDGeneric curSubsetSize [asSome . categoricalVariables, asSome . integralVariables]
      endScore <- gets currentScore
      logInfoN [i|Full optimization step done, went from #{startScore} to #{endScore}|]
      if startScore /= endScore
        then fixGD (subtract 1 <$> counter) 1
        else do
          logInfoN [i|Done optimizing with subset size #{curSubsetSize}, stopped at score #{endScore}|]
          fixGD (subtract 1 <$> counter) (curSubsetSize + 1)
