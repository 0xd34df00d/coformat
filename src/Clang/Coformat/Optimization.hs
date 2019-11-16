{-# LANGUAGE GADTs, DataKinds, TypeApplications, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes, RecordWildCards, LambdaCase #-}

module Clang.Coformat.Optimization where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Concurrent.Async
import Control.Lens
import Control.Monad.Except
import Control.Monad.Logger
import Data.List
import Data.Ord
import Data.Proxy
import Data.String.Interpolate.IsString
import System.Command.QQ

import Clang.Coformat.StyOpts
import Clang.Coformat.Util
import Clang.Format.Descr
import Text.Levenshteins

forConcurrently' :: (MonadLoggerIO m, MonadError e m)
                 => [a]
                 -> (forall m'. (MonadLoggerIO m', MonadError e m') => a -> m' b)
                 -> m [b]
forConcurrently' lst act = do
  logger <- askLoggerIO
  result <- liftIO $ forConcurrently lst $ \elt -> flip runLoggingT logger $ runExceptT $ act elt
  liftEither $ sequence result

chooseBaseStyle :: (MonadError String m, MonadLoggerIO m) => [T.Text] -> [String] -> m T.Text
chooseBaseStyle baseStyles files = do
  sty2dists <- forConcurrently' ((,) <$> baseStyles <*> files) $ \(sty, file) -> do
    let formattedSty = formatStyArg StyOpts { basedOnStyle = sty, overriddenOpts = [] }
    stdout <- checked [sh|clang-format --style="#{formattedSty}" #{file}|]
    source <- liftIO $ readFile file
    let dist = levenshteinDistanceWith (blindTokens . dropStartSpaces) source $ TL.unpack stdout
    logDebugN [i|Initial guess for #{sty} at #{file}: #{dist}|]
    pure (sty, dist)
  let accumulated = HM.toList $ HM.fromListWith (+) sty2dists
  forM_ accumulated $ \(sty, acc) -> logInfoN [i|Initial accumulated guess for #{sty}: #{acc}|]
  pure $ fst $ minimumBy (comparing snd) accumulated

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

stepVC :: (MonadLoggerIO m, MonadError String m)
       => T.Text -> [String] -> [(DiscreteVariable, Int)] -> [ConfigItemT 'Value] -> m [ConfigItemT 'Value]
stepVC baseStyle files dvs opts = do
  results <- forConcurrently' dvs $ \(MkDV (_ :: a), idx) -> do
    let optName = name $ opts !! idx
    opt2dists <- forM (variateAt @a Proxy idx opts) $ \opts' -> do
      let optValue = typ $ opts' !! idx
      let formattedSty = formatStyArg StyOpts { basedOnStyle = baseStyle, overriddenOpts = opts' }
      dists <- forM files $ \file -> do
        stdout <- checked [sh|clang-format --style="#{formattedSty}" #{file}|]
        source <- liftIO $ readFile file
        let dist = levenshteinDistanceWith (blindTokens . dropStartSpaces) source $ TL.unpack stdout
        logDebugN [i|Variate guess for #{optName}=#{optValue} at #{file}: #{dist}|]
        pure dist
      logDebugN [i|Total dist for #{optName}=#{optValue}: #{sum dists}|]
      pure (optValue, sum dists)
    let (bestOptVal, bestSum) = minimumBy (comparing snd) opt2dists
    logDebugN [i|Best step for #{optName}: #{bestOptVal} at #{bestSum}|]
    logDebugN [i|Best step for #{optName}: #{bestOptVal} at #{bestSum}|]
    pure (bestOptVal, bestSum, idx)
  let (bestOptVal, bestSum, bestIdx) = minimumBy (comparing (^._2)) results
  logDebugN [i|Overall best step: change #{name $ opts !! bestIdx} to #{bestOptVal} (gives #{bestSum})|]
  pure $ update bestIdx (\cfg -> cfg { typ = bestOptVal }) opts

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
