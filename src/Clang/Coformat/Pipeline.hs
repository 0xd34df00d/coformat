{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE TypeApplications, OverloadedStrings, RecordWildCards, QuasiQuotes #-}

module Clang.Coformat.Pipeline
( runOptPipeline
, PipelineOpts(..)
) where

import qualified Control.Monad.Except.CoHas as EC
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Control.Concurrent.Async.Pool
import Control.Lens hiding (Wrapped, Unwrapped)
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Aeson.Lens
import Data.Foldable
import Data.List.NonEmpty(NonEmpty)
import Data.Maybe
import Data.String.Interpolate.IsString
import Data.Traversable
import Numeric.Natural

import Clang.Coformat.Optimization
import Clang.Coformat.Score
import Clang.Coformat.StyOpts
import Clang.Coformat.Util
import Clang.Coformat.Variables
import Clang.Format.Descr
import Clang.Format.Descr.Operations
import Clang.Format.YamlConversions
import Language.Coformat.Formatter

data InitializeOptionsResult = InitializeOptionsResult
  { baseStyle :: T.Text
  , baseScore :: Score
  , baseOptions :: [ConfigItemT 'Value]
  , filledOptions :: [ConfigItemT 'Value]
  , userForcedOpts :: [ConfigItemT 'Value]
  }

initializeOptions :: (MonadError String m, MonadLoggerIO m)
                  => SomeFormatter -> [PreparedFile] -> Maybe FilePath -> [String] -> m InitializeOptionsResult
initializeOptions (SomeFormatter formatter) preparedFiles maybeResumePath forceStrs = do
  OptsDescription { .. } <- parseOpts formatterOpts >>= liftEither
  let varyingOptions = filter (not . (`elem` hardcodedOptsNames) . name) knownOpts
  userForcedOpts <- parseUserOpts forceStrs knownOpts

  maybeResumeObj <- for maybeResumePath $ liftIO . BS.readFile
                                      >=> convert (show @FillError) . preprocessYaml PartialConfig
  maybeResumeOptions <- for maybeResumeObj $ convert (show @FillError) . collectConfigItems varyingOptions

  let allFixedOpts = hardcodedOpts <> userForcedOpts

  (baseStyle, baseScore) <-
      case maybeResumeObj of
           Nothing -> chooseBaseStyle baseStyles allFixedOpts preparedFiles
           Just resumeObj -> do
              baseStyle <- EC.liftMaybe ("Unable to find `BasedOnStyle` key in the resume file" :: String)
                        $ HM.lookup "BasedOnStyle" resumeObj ^? _Just . _String
              constantOpts <- convert (show @FillError) $ collectConfigItems varyingOptions resumeObj
              score <- convert (show @Failure) $ flip runReaderT FmtEnv { .. } $ runClangFormatFiles allFixedOpts [i|Calculating the score of the resumed-from style|]
              pure (baseStyle, score)

  logInfoN [i|Using initial style: #{baseStyle} with score of #{baseScore}|]
  let formattedBaseSty = formatStyArg $ StyOpts { basedOnStyle = baseStyle, additionalOpts = allFixedOpts }
  stdout <- convert (show @Failure) $ runCommand Cmd { exec = "clang-format", args = ["--style=" <> formattedBaseSty, "--dump-config"] }
  baseOptions <- convert (show @FillError) $ fillConfigItems varyingOptions stdout

  let filledOptions | Just resumeOptions <- maybeResumeOptions = baseOptions `replaceItemsWith` resumeOptions
                    | otherwise = baseOptions

  pure InitializeOptionsResult { .. }
  where
    hardcodedOptsNames = name <$> hardcodedOpts
    FormatterInfo { .. } = formatterInfo formatter

parseUserOpts :: (MonadError String m, ParseableConfigState f) => [String] -> [ConfigItemT f] -> m [ConfigItemT 'Value]
parseUserOpts opts baseOpts = forM opts $ splitStr >=> findBaseOpt >=> uncurry parseConfigValue
  where
    splitStr str | (name, _:valStr) <- break (== ':') str = pure (T.splitOn "." $ T.pack name, valStr)
                 | otherwise = throwError [i|Unable to parse `#{str}`: it should have the form of `name:value`|]
    findBaseOpt (name, valStr) | Just item <- HM.lookup name baseOptsMap = pure (item, valStr)
                               | otherwise = throwError [i|Unable to find option `#{name}`|]

    baseOptsMap = HM.fromList [ (name item, item) | item <- baseOpts]

data PipelineOpts = PipelineOpts
  { maxSubsetSize :: Maybe Natural
  , resumePath :: Maybe FilePath
  , input :: NonEmpty FilePath
  , taskGroup :: TaskGroup
  , forceStrs :: [String]
  , formatter :: SomeFormatter
  }

runOptPipeline :: (MonadError String m, MonadLoggerIO m)
               => PipelineOpts -> m BS.ByteString
runOptPipeline PipelineOpts { .. } = do
  preparedFiles <- mapM prepareFile $ toList input

  InitializeOptionsResult { .. } <- initializeOptions formatter preparedFiles resumePath forceStrs

  let categoricalVariables = [ IxedVariable dv idx
                             | (Just dv, idx) <- zip (typToDV . value <$> filledOptions) [0..]
                             ]
  let integralVariables = [ IxedVariable dv idx
                          | (Just dv, idx) <- zip (typToIV . value <$> filledOptions) [0..]
                          ]
  let constantOpts = hardcodedOpts (someFormatterInfo formatter) <> userForcedOpts
  let fmtEnv = FmtEnv { .. }
  let optEnv = OptEnv { maxSubsetSize = fromMaybe 1 maxSubsetSize, .. }
  let optState = initOptState filledOptions baseScore
  finalOptState <- convert (show @UnexpectedFailure) $ flip runReaderT (fmtEnv, optEnv, taskGroup) $ execStateT (fixGD Nothing 1) optState
  let finalStyOpts = StyOpts { basedOnStyle = baseStyle
                             , additionalOpts = constantOpts <> currentOpts finalOptState `subtractMatching` baseOptions
                             }
  pure $ formatClangFormat finalStyOpts
