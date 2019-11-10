{-# LANGUAGE GADTs, DataKinds #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes, RecordWildCards #-}

module Clang.Coformat.Optimization where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Concurrent.Async
import Control.Monad.Except
import Control.Monad.Logger
import Data.List
import Data.Ord
import Data.String.Interpolate.IsString
import System.Command.QQ

import Clang.Coformat.StyOpts
import Clang.Coformat.Util
import Clang.Format.Descr
import Text.Levenshteins

chooseBaseStyle :: (MonadError String m, MonadLoggerIO m) => [ConfigItemT 'Value] -> [T.Text] -> [String] -> m T.Text
chooseBaseStyle ufos baseStyles files = do
  logger <- askLoggerIO
  estimates <- liftIO $ forConcurrently ((,) <$> baseStyles <*> files) $ \(sty, file) -> flip runLoggingT logger $ runExceptT $ do
    let formattedSty = formatStyArg StyOpts { basedOnStyle = sty, overriddenOpts = ufos }
    stdout <- checked [sh|clang-format --style="#{formattedSty}" #{file}|]
    source <- liftIO $ readFile file
    let dist = levenshteinDistanceWith (blindTokens . dropStartSpaces) source $ TL.unpack stdout
    logDebugN [i|Initial guess for #{sty} at #{file}: #{dist}|]
    pure (sty, dist)
  sty2dists <- liftEither $ sequence estimates
  let accumulated = HM.toList $ HM.fromListWith (+) sty2dists
  forM_ accumulated $ \(sty, acc) -> logInfoN [i|Initial accumulated guess for #{sty}: #{acc}|]
  pure $ fst $ minimumBy (comparing snd) accumulated
