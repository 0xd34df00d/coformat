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
import Data.Maybe
import Data.String.Interpolate.IsString
import GHC.Conc
import GHC.Generics
import Numeric.Natural
import Options.Generic
import System.Command.QQ
import System.IO(IOMode(..), Handle, stderr, withFile)
import System.Log.FastLogger

import Clang.Coformat.Optimization
import Clang.Coformat.Score
import Clang.Coformat.StyOpts
import Clang.Coformat.Util
import Clang.Coformat.Variables
import Clang.Format.Descr
import Clang.Format.DescrParser
import Clang.Format.YamlConversions

liftEither' :: (MonadError String m, Show e) => String -> Either e a -> m a
liftEither' context = liftEither . first ((context <>) . show)

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
  { parallelism :: w ::: Maybe Natural <?> "Max parallel threads of heavy-duty computations (defaults to NCPUs - 1)"
  , debugLog :: w ::: Maybe FilePath <?> "Debug log file (disabled by default)"
  , maxSubsetSize :: w ::: Maybe Natural <?> "Maximum size of the inter-dependent subsets to consider (defaults to 1)"
  , input :: w ::: N.NonEmpty FilePath <?> "The input file(s) to use"
  , output :: w ::: FilePath <?> "Where to save the resulting configuration file"
  } deriving (Generic)

instance ParseRecord (Options Wrapped)

hardcodedOpts :: [ConfigItemT 'Value]
hardcodedOpts = [ ConfigItem { name = ["Language"], typ = CTEnum ["Cpp"] "Cpp" }
                , ConfigItem { name = ["BreakBeforeBraces"], typ = CTEnum ["Custom"] "Custom" }
                , ConfigItem { name = ["DisableFormat"], typ = CTBool False }
                , ConfigItem { name = ["SortIncludes"], typ = CTBool False }
                ]

initializeOptions :: (MonadError String m, MonadLoggerIO m)
                  => [PreparedFile] -> m (T.Text, Score, [ConfigItemT 'Value])
initializeOptions preparedFiles = do
  (baseStyles, allOptions) <- parseOptsDescription "data/ClangFormatStyleOptions-9.html"
  let varyingOptions = filter (not . (`elem` hardcodedOptsNames) . name) allOptions
  (baseStyle, baseScore) <- chooseBaseStyle baseStyles hardcodedOpts preparedFiles
  logInfoN [i|Using initial style: #{baseStyle} with score of #{baseScore}|]
  stdout <- convert (show @Failure) $ checked [sh|clang-format --style=#{baseStyle} --dump-config|]
  filledOptions <- convert (show @FillError) $ fillConfigItems varyingOptions $ BSL.toStrict $ TL.encodeUtf8 stdout
  pure (baseStyle, baseScore, filledOptions)
  where
    hardcodedOptsNames = name <$> hardcodedOpts

runOptPipeline :: (MonadError String m, MonadLoggerIO m) => Natural -> TaskGroup -> [FilePath] -> m BS.ByteString
runOptPipeline maxSubsetSize tg files = do
  preparedFiles <- mapM prepareFile files

  (baseStyle, baseScore, filledOptions) <- initializeOptions preparedFiles

  let categoricalVariables = [ IxedVariable dv idx
                             | (Just dv, idx) <- zip (typToDV . typ <$> filledOptions) [0..]
                             ]
  let integralVariables = [ IxedVariable dv idx
                          | (Just dv, idx) <- zip (typToIV . typ <$> filledOptions) [0..]
                          ]
  let optEnv = OptEnv { constantOpts = hardcodedOpts, .. }
  let optState = initOptState filledOptions baseScore
  finalOptState <- convert (show @UnexpectedFailure) $ flip runReaderT (optEnv, tg) $ execStateT (fixGD Nothing 1) optState
  pure $ formatClangFormat $ StyOpts { basedOnStyle = baseStyle, additionalOpts = hardcodedOpts <> currentOpts finalOptState }

logOutput :: Maybe Handle
          -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
logOutput debugHandle loc src level msg
  | level == LevelDebug = case debugHandle of Nothing -> pure ()
                                              Just handle -> BS.hPutStr handle str
  | otherwise = BS.hPutStr stderr str
  where
    str = fromLogStr $ defaultLogStr loc src level msg

main :: IO ()
main = do
  Options { .. } <- unwrapRecord "coformat"

  tgSize <- case parallelism of
                 Just n -> pure $ fromIntegral n
                 Nothing -> (\n -> max 1 $ n - 1) <$> getNumCapabilities

  let withDebugLog | Just path <- debugLog = \f -> withFile path AppendMode $ f . Just
                   | otherwise = ($ Nothing)

  res <- withDebugLog $ \maybeLogHandle ->
         withTaskGroup tgSize $ \tg ->
         (`runLoggingT` logOutput maybeLogHandle) $ runExceptT $ runOptPipeline (fromMaybe 1 maxSubsetSize) tg $ toList input
  case res of
       Left err -> putStrLn err
       Right bs -> BS.writeFile output bs
