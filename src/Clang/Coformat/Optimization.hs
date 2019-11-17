{-# LANGUAGE GADTs, DataKinds, TypeApplications, RankNTypes, ScopedTypeVariables, ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes, RecordWildCards, LambdaCase, TupleSections #-}

module Clang.Coformat.Optimization where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Concurrent.Async
import Control.Lens
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader.Has hiding(update)
import Control.Monad.State.Strict
import Data.List
import Data.Maybe
import Data.Ord
import Data.Proxy
import Data.String.Interpolate.IsString
import System.Command.QQ

import Clang.Coformat.StyOpts
import Clang.Coformat.Util
import Clang.Format.Descr
import Text.Levenshteins

data OptEnv = OptEnv
  { baseStyle :: T.Text
  , files :: [String]
  , discreteVariables :: [IxedDiscreteVariable]
  }

forConcurrently' :: (MonadLoggerIO m, MonadError e m)
                 => [a]
                 -> (forall m'. (MonadLoggerIO m', MonadError e m') => a -> m' b)
                 -> m [b]
forConcurrently' lst act = do
  logger <- askLoggerIO
  result <- liftIO $ forConcurrently lst $ \elt -> flip runLoggingT logger $ runExceptT $ act elt
  liftEither $ sequence result

newtype Score = Score { getScore :: Int } deriving (Eq, Ord, Show, Num)

runClangFormat :: (MonadError String m, MonadIO m, MonadLogger m)
               => String -> String -> BSL.ByteString -> m Score
runClangFormat file logStr formattedSty = do
  let unpackedSty = BSL.unpack formattedSty
  stdout <- checked [sh|clang-format --style='#{unpackedSty}' #{file}|]
  source <- liftIO $ readFile file
  let dist = levenshteinDistanceWith (blindTokens . dropStartSpaces) source $ TL.unpack stdout
  logDebugN [i|#{logStr}: #{dist}|]
  pure $ Score dist

runClangFormatFiles :: (MonadError String m, MonadIO m, MonadLogger m)
                    => [String] -> String -> StyOpts -> m Score
runClangFormatFiles files logStr sty = fmap sum $ forM files $ \file -> runClangFormat file [i|#{logStr} at #{file}|] formattedSty
  where
    formattedSty = formatStyArg sty

chooseBaseStyle :: (MonadError String m, MonadLoggerIO m) => [T.Text] -> [String] -> m (T.Text, Score)
chooseBaseStyle baseStyles files = do
  sty2dists <- forConcurrently' ((,) <$> baseStyles <*> files) $ \(sty, file) ->
    (sty,) <$> runClangFormat file [i|Initial guess for #{sty} at #{file}|] (formatStyArg StyOpts { basedOnStyle = sty, additionalOpts = [] })
  let accumulated = HM.toList $ HM.fromListWith (+) sty2dists
  forM_ accumulated $ \(sty, acc) -> logInfoN [i|Initial accumulated guess for #{sty}: #{acc}|]
  pure $ minimumBy (comparing snd) accumulated

update :: Int -> (a -> a) -> [a] -> [a]
update idx f = zipWith z [0..]
  where z idx' val = if idx' == idx then f val else val

variateAt :: forall a. DiscreteVariate a
          => Proxy a -> Int -> [ConfigItemT 'Value] -> [[ConfigItemT 'Value]]
variateAt _ idx opts = [ update idx (updater v') opts | v' <- variated ]
  where
    thePrism :: Prism' (ConfigTypeT 'Value) a
    thePrism = varPrism
    variated = variate $ typ (opts !! idx) ^?! thePrism
    updater v cfg = cfg { typ = typ cfg & thePrism .~ v }

type OptMonad r m = (MonadLoggerIO m, MonadError String m, MonadReader r m, Has OptEnv r)

data OptState = OptState
  { currentOpts :: [ConfigItemT 'Value]
  , currentScore :: Score
  } deriving (Show)

chooseBestOptVals :: (OptMonad r m, Has OptState r)
                  => m [(ConfigTypeT 'Value, Score, Int)]
chooseBestOptVals = do
  OptEnv { .. } <- ask
  OptState { .. } <- ask
  partialResults <- forConcurrently' discreteVariables $ \(IxedDiscreteVariable (MkDV (_ :: a)) idx) -> do
    let optName = name $ currentOpts !! idx
    opt2dists <- forM (variateAt @a Proxy idx currentOpts) $ \opts' -> do
      let optValue = typ $ opts' !! idx
      sumScore <- runClangFormatFiles files [i|Variate guess for #{optName}=#{optValue}|] StyOpts { basedOnStyle = baseStyle, additionalOpts = opts' }
      logDebugN [i|Total dist for #{optName}=#{optValue}: #{sumScore}|]
      pure (optValue, sumScore)
    let (bestOptVal, bestScore) = minimumBy (comparing snd) opt2dists
    logDebugN [i|Best step for #{optName}: #{bestOptVal} at #{bestScore} (compare to #{currentScore}|]
    pure $ if bestScore < currentScore
            then Just (bestOptVal, bestScore, idx)
            else Nothing
  pure $ catMaybes partialResults

stepGD :: (OptMonad r m, MonadState OptState m) => m Score
stepGD = do
  current <- get
  env@OptEnv { .. } <- ask
  results <- runReaderT chooseBestOptVals (current, env)
  let nextOpts = foldr (\(val, _, idx) -> update idx (\cfg -> cfg { typ = val })) (currentOpts current) results
  sumScore <- runClangFormatFiles files [i|Total score after optimization|] StyOpts { basedOnStyle = baseStyle, additionalOpts = nextOpts }
  put OptState { currentOpts = nextOpts, currentScore = sumScore }
  pure sumScore

class DiscreteVariate a where
  variate :: a -> [a]
  varPrism :: Prism' (ConfigTypeT 'Value) a

instance DiscreteVariate Bool where
  variate b = [not b]
  varPrism = prism' CTBool $ \case CTBool b -> Just b
                                   _ -> Nothing

instance DiscreteVariate ([T.Text], T.Text) where
  variate (vars, cur) = [(vars, next) | next <- vars, next /= cur]
  varPrism = prism' (uncurry CTEnum) $ \case CTEnum vars cur -> Just (vars, cur)
                                             _ -> Nothing

data DiscreteVariable where
  -- TODO proxy should be enough
  MkDV :: DiscreteVariate a => a -> DiscreteVariable

typToDV :: ConfigTypeT 'Value -> Maybe DiscreteVariable
typToDV val = msum [ MkDV <$> val ^? varPrism @Bool
                   , MkDV <$> val ^? varPrism @([T.Text], T.Text)
                   ]

data IxedDiscreteVariable = IxedDiscreteVariable
  { discreteVar :: DiscreteVariable
  , varIdx :: Int
  }
