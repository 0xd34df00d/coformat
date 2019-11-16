{-# LANGUAGE GADTs, DataKinds, TypeApplications, RankNTypes #-}
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
