{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications, OverloadedStrings, RecordWildCards, QuasiQuotes #-}

module Clang.Coformat.Pipeline
( runOptPipeline
, PipelineOpts(..)
) where

import qualified Control.Monad.Except.CoHas as EC
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TL
import Control.Concurrent.Async.Pool
import Control.Lens hiding (Wrapped, Unwrapped)
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Aeson.Lens
import Data.Bifunctor
import Data.Foldable
import Data.List.NonEmpty(NonEmpty)
import Data.Maybe
import Data.String.Interpolate.IsString
import Data.Traversable
import Numeric.Natural
import System.Command.QQ

import Clang.Coformat.Optimization
import Clang.Coformat.Score
import Clang.Coformat.StyOpts
import Clang.Coformat.Util
import Clang.Coformat.Variables
import Clang.Format.Descr
import Clang.Format.Descr.Operations
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

hardcodedOpts :: [ConfigItemT 'Value]
hardcodedOpts = [ ConfigItem { name = ["Language"], typ = CTEnum ["Cpp"] "Cpp" }
                , ConfigItem { name = ["BreakBeforeBraces"], typ = CTEnum ["Custom"] "Custom" }
                , ConfigItem { name = ["DisableFormat"], typ = CTBool False }
                , ConfigItem { name = ["SortIncludes"], typ = CTBool False }
                ]

data InitializeOptionsResult = InitializeOptionsResult
  { baseStyle :: T.Text
  , baseScore :: Score
  , filledOptions :: [ConfigItemT 'Value]
  }

initializeOptions :: (MonadError String m, MonadLoggerIO m)
                  => [PreparedFile] -> Maybe FilePath -> m InitializeOptionsResult
initializeOptions preparedFiles maybeResumePath = do
  (baseStyles, allOptions) <- parseOptsDescription "data/ClangFormatStyleOptions-9.html"
  let varyingOptions = filter (not . (`elem` hardcodedOptsNames) . name) allOptions

  maybeResumeObj <- for maybeResumePath $ liftIO . BS.readFile
                                      >=> convert (show @FillError) . preprocessYaml PartialConfig
  maybeResumeOptions <- for maybeResumeObj $ convert (show @FillError) . collectConfigItems varyingOptions

  (baseStyle, baseScore) <-
      case maybeResumeObj of
           Nothing -> chooseBaseStyle baseStyles hardcodedOpts preparedFiles
           Just resumeObj -> do
              baseStyle <- EC.liftMaybe ("Unable to find `BasedOnStyle` key in the resume file" :: String)
                        $ HM.lookup "BasedOnStyle" resumeObj ^? _Just . _String
              constantOpts <- convert (show @FillError) $ collectConfigItems varyingOptions resumeObj
              score <- convert (show @Failure) $ flip runReaderT FmtEnv { .. } $ runClangFormatFiles hardcodedOpts [i|Calculating the score of the resumed-from style|]
              pure (baseStyle, score)

  logInfoN [i|Using initial style: #{baseStyle} with score of #{baseScore}|]
  stdout <- convert (show @Failure) $ checked [sh|clang-format --style=#{baseStyle} --dump-config|]
  baseOptions <- convert (show @FillError) $ fillConfigItems varyingOptions $ BSL.toStrict $ TL.encodeUtf8 stdout

  let filledOptions | Just resumeOptions <- maybeResumeOptions = baseOptions `replaceItemsWith` resumeOptions
                    | otherwise = baseOptions

  pure InitializeOptionsResult { .. }
  where
    hardcodedOptsNames = name <$> hardcodedOpts

data PipelineOpts = PipelineOpts
  { maxSubsetSize :: Maybe Natural
  , resumePath :: Maybe FilePath
  , input :: NonEmpty FilePath
  , taskGroup :: TaskGroup
  }

runOptPipeline :: (MonadError String m, MonadLoggerIO m)
               => PipelineOpts -> m BS.ByteString
runOptPipeline PipelineOpts { .. } = do
  preparedFiles <- mapM prepareFile $ toList input

  InitializeOptionsResult { .. } <- initializeOptions preparedFiles resumePath

  let categoricalVariables = [ IxedVariable dv idx
                             | (Just dv, idx) <- zip (typToDV . typ <$> filledOptions) [0..]
                             ]
  let integralVariables = [ IxedVariable dv idx
                          | (Just dv, idx) <- zip (typToIV . typ <$> filledOptions) [0..]
                          ]
  let fmtEnv = FmtEnv { constantOpts = hardcodedOpts, .. }
  let optEnv = OptEnv { maxSubsetSize = fromMaybe 1 maxSubsetSize, .. }
  let optState = initOptState filledOptions baseScore
  finalOptState <- convert (show @UnexpectedFailure) $ flip runReaderT (fmtEnv, optEnv, taskGroup) $ execStateT (fixGD Nothing 1) optState
  pure $ formatClangFormat $ StyOpts { basedOnStyle = baseStyle, additionalOpts = hardcodedOpts <> currentOpts finalOptState }
