{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeOperators, TypeApplications #-}
{-# LANGUAGE DataKinds, RankNTypes, GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Data.List.NonEmpty as N
import Control.Concurrent.Async.Pool
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Trans.Maybe
import Data.Maybe
import GHC.Conc
import GHC.Generics
import Numeric.Natural
import Options.Generic
import System.Directory.Extra
import System.IO(IOMode(..), Handle, stderr, withFile)
import System.Log.FastLogger

import Clang.Format.Formatter
import Language.Coformat.Formatter
import Language.Coformat.Formatter.Failure
import Language.Coformat.Pipeline
import Language.Coformat.Util

data Options w = Options
  { parallelism :: w ::: Maybe Natural <?> "Max parallel threads of heavy-duty computations (defaults to NCPUs - 1)"
  , debugLog :: w ::: Maybe FilePath <?> "Debug log file (disabled by default)"
  , maxSubsetSize :: w ::: Maybe Natural <?> "Maximum size of the inter-dependent subsets to consider (defaults to 1)"
  , resumePath :: w ::: Maybe FilePath <?> "The path to the style format file to start from (if any)"
  , forceOption :: w ::: [String] <?> "Force these options to have the given values (`option:value`)"
  , input :: w ::: N.NonEmpty FilePath <?> "The input file(s) to use"
  , output :: w ::: FilePath <?> "Where to save the resulting configuration file"
  } deriving (Generic)

instance ParseRecord (Options Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

logOutput :: Maybe Handle
          -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
logOutput debugHandle loc src level msg
  | level == LevelDebug = case debugHandle of Nothing -> pure ()
                                              Just handle -> BS.hPutStr handle str
  | otherwise = BS.hPutStr stderr str
  where
    str = fromLogStr $ defaultLogStr loc src level msg

getAvailableFormatters :: IO [Formatter]
getAvailableFormatters = filterM (flt . formatterInfo) [clangFormatter ClangFormat10, clangFormatter ClangFormat9]
  where
    flt FormatterInfo { .. } = fmap (fromMaybe False) $ runMaybeT $ do
      void $ lift $ findExecutable execName
      output <- exceptToMaybeT $ convert (show @Failure) $ runCommand execName verCmd
      pure $ verChecker output
      where
        (verCmd, verChecker) = versionCheck

main :: IO ()
main = do
  availableFormatters <- getAvailableFormatters
  putStrLn "Available formatters:"
  forM_ availableFormatters $ \fmt -> putStrLn $ " * " <> formatterName (formatterInfo fmt)

  Options { .. } <- unwrapRecord "coformat"

  tgSize <- case parallelism of
                 Just n -> pure $ fromIntegral n
                 Nothing -> (\n -> max 1 $ n - 1) <$> getNumCapabilities

  let withDebugLog | Just path <- debugLog = \f -> withFile path AppendMode $ f . Just
                   | otherwise = ($ Nothing)

  res <- withDebugLog $ \maybeLogHandle ->
         withTaskGroup tgSize $ \taskGroup ->
         (`runLoggingT` logOutput maybeLogHandle) $ runExceptT $ runOptPipeline PipelineOpts
                                                                                { forceStrs = forceOption
                                                                                , formatter = clangFormatter ClangFormat9
                                                                                , ..
                                                                                }
  case res of
       Left err -> putStrLn err
       Right bs -> BS.writeFile output bs
