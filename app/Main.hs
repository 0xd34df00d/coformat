{-# LANGUAGE FlexibleContexts, TypeApplications #-}
{-# LANGUAGE DataKinds, RankNTypes, GADTs #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, QuasiQuotes #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TL
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Foldable
import Data.String.Interpolate.IsString
import System.Command.QQ

import Clang.Coformat.Optimization
import Clang.Coformat.Util
import Clang.Format.Descr
import Clang.Format.DescrParser
import Clang.Format.YamlParser

liftEither' :: (MonadError String m, Show e) => String -> Either e a -> m a
liftEither' context = liftEither . first ((context <>) . show)

convert :: (MonadError String m, MonadIO m, Show e)
        => (e -> String)
        -> (forall m'. (MonadError e m', MonadIO m') => m' a)
        -> m a
convert cvt act = runExceptT act >>= (liftEither . first cvt)

parseOptsDescription :: (MonadError String m, MonadIO m) => FilePath -> m ([T.Text], [ConfigItemT 'Supported])
parseOptsDescription path = do
  parseResult <- liftIO $ parseFile path
  supportedOptions <- filterParsedItems <$> liftEither' "Unable to parse the file: " parseResult
  baseStyles <- case find ((== bosKey) . name) supportedOptions of
                     Nothing -> throwError "No `BasedOnStyle` option"
                     Just stys -> pure stys
  let varyingOptions = filter ((/= bosKey) . name) supportedOptions
  case typ baseStyles of
       CTEnum { .. } -> pure (variants, varyingOptions)
       _ -> throwError [i|Unknown type for the `BaseStyles` option: #{typ baseStyles}|]
  where
    bosKey = ["BasedOnStyle"]

newtype Options = Options
  { inputFiles :: [String]
  } deriving (Eq, Show)

doWork :: (MonadError String m, MonadLoggerIO m) => m ()
doWork = do
  (baseStyles, varyingOptions) <- parseOptsDescription "data/ClangFormatStyleOptions-9.html"
  let files = ["data/core.cpp", "data/core.h"]
  (baseStyle, baseScore) <- chooseBaseStyle baseStyles files
  logInfoN [i|Using initial style: #{baseStyle} with score of #{baseScore}|]
  stdout <- checked [sh|clang-format --style=#{baseStyle} --dump-config|]
  filledOptions <- convert (show @FillError) $ fillConfigItems varyingOptions $ BSL.toStrict $ TL.encodeUtf8 stdout
  let discreteVariables = [ IxedDiscreteVariable dv idx
                          | (Just dv, idx) <- zip (typToDV . typ <$> filledOptions) [0..]
                          ]
  let optEnv = OptEnv { .. }
  let optState = OptState { currentOpts = filledOptions, currentScore = baseScore }
  res <- flip runReaderT optEnv $ runStateT stepGD optState
  liftIO $ print res

main :: IO ()
main = do
  res <- runStderrLoggingT $ runExceptT doWork
  case res of
       Left err -> putStrLn err
       Right () -> putStrLn "done"
