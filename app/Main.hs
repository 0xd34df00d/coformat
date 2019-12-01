{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeApplications, TypeOperators #-}
{-# LANGUAGE DataKinds, RankNTypes, GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, QuasiQuotes #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.List.NonEmpty as N
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TL
import Control.Concurrent.Async.Pool
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Foldable
import Data.String.Interpolate.IsString
import GHC.Conc
import GHC.Generics
import Numeric.Natural
import Options.Generic
import System.Command.QQ

import Clang.Coformat.Optimization
import Clang.Coformat.StyOpts
import Clang.Coformat.Util
import Clang.Coformat.Variables
import Clang.Format.Descr
import Clang.Format.DescrParser
import Clang.Format.YamlConversions

liftEither' :: (MonadError String m, Show e) => String -> Either e a -> m a
liftEither' context = liftEither . first ((context <>) . show)

convert :: MonadError e' m
        => (e -> e')
        -> ExceptT e m a
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

data Options w = Options
  { input :: w ::: N.NonEmpty FilePath <?> "The input file(s) to use"
  , parallelism :: w ::: Maybe Natural <?> "Max parallel threads of heavy-duty computations (defaults to NCPUs - 1)"
  } deriving (Generic)

instance ParseRecord (Options Wrapped)

runOptPipeline :: (MonadError String m, MonadLoggerIO m) => TaskGroup -> [FilePath] -> m ()
runOptPipeline tg files = do
  (baseStyles, allOptions) <- parseOptsDescription "data/ClangFormatStyleOptions-9.html"
  let varyingOptions = filter (not . (`elem` constantOptsNames) . name) allOptions
  (baseStyle, baseScore) <- chooseBaseStyle baseStyles files
  logInfoN [i|Using initial style: #{baseStyle} with score of #{baseScore}|]
  stdout <- checked [sh|clang-format --style=#{baseStyle} --dump-config|]
  filledOptions <- convert (show @FillError) $ fillConfigItems varyingOptions $ BSL.toStrict $ TL.encodeUtf8 stdout
  let categoricalVariables = [ IxedVariable dv idx
                             | (Just dv, idx) <- zip (typToDV . typ <$> filledOptions) [0..]
                             ]
  let integralVariables = [ IxedVariable dv idx
                          | (Just dv, idx) <- zip (typToIV . typ <$> filledOptions) [0..]
                          ]
  let optEnv = OptEnv { .. }
  let optState = initOptState filledOptions baseScore
  finalOptState <- flip runReaderT (optEnv, tg) $ execStateT (fixGD $ Just 10) optState
  liftIO $ BS.putStrLn $ formatClangFormat $ StyOpts { basedOnStyle = baseStyle, additionalOpts = constantOpts <> currentOpts finalOptState }
  where
    constantOpts = [ ConfigItem { name = ["Language"], typ = CTEnum ["Cpp"] "Cpp" }
                   , ConfigItem { name = ["BreakBeforeBraces"], typ = CTEnum ["Custom"] "Custom" }
                   , ConfigItem { name = ["DisableFormat"], typ = CTBool False }
                   ]
    constantOptsNames = name <$> constantOpts

main :: IO ()
main = do
  Options { .. } <- unwrapRecord "coformat"
  tgSize <- case parallelism of
                 Just n -> pure $ fromIntegral n
                 Nothing -> (\n -> max 1 $ n - 1) <$> getNumCapabilities
  res <- withTaskGroup tgSize $ \tg -> runStderrLoggingT $ runExceptT $ runOptPipeline tg $ toList input
  case res of
       Left err -> putStrLn err
       Right () -> putStrLn "done"
