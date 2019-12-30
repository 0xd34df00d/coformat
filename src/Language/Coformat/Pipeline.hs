{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE TypeApplications, OverloadedStrings, RecordWildCards, QuasiQuotes #-}

module Language.Coformat.Pipeline
( runOptPipeline
, PipelineOpts(..)
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Control.Concurrent.Async.Pool
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Foldable
import Data.List.NonEmpty(NonEmpty)
import Data.Maybe
import Data.String.Interpolate.IsString
import Data.Traversable
import Numeric.Natural

import Language.Coformat.Descr
import Language.Coformat.Descr.Operations
import Language.Coformat.Formatter
import Language.Coformat.Optimization
import Language.Coformat.Score
import Language.Coformat.Util
import Language.Coformat.Variables

data InitializeOptionsResult = InitializeOptionsResult
  { baseStyle :: T.Text
  , baseScore :: Score
  , baseOptions :: [ConfigItemT 'Value]
  , filledOptions :: [ConfigItemT 'Value]
  , userForcedOpts :: [ConfigItemT 'Value]
  }

initializeOptions :: (MonadError String m, MonadLoggerIO m)
                  => Formatter -> [PreparedFile] -> Maybe FilePath -> [String] -> m InitializeOptionsResult
initializeOptions Formatter { formatterInfo = formatterInfo@FormatterInfo { .. }, .. } preparedFiles maybeResumePath forceStrs = do
  OptsDescription { .. } <- parseOpts execName formatterOpts
  let varyingOptions = filter (not . (`elem` hardcodedOptsNames) . name) knownOpts
  userForcedOpts <- parseUserOpts forceStrs knownOpts

  let allFixedOpts = hardcodedOpts <> userForcedOpts

  maybeResumeObj <- for maybeResumePath $ liftIO . BS.readFile >=> liftEither . parseResumeObject
  maybeResumeOptions <- for maybeResumeObj $ liftEither . parseResumeOptions varyingOptions

  (baseStyle, baseScore) <-
      case maybeResumeOptions of
           Nothing -> chooseBaseStyle formatterInfo baseStyles allFixedOpts preparedFiles
           Just (baseStyle, constantOpts) -> do
              let fmtAct = runFormatFiles allFixedOpts [i|Calculating the score of the resumed-from style|]
              score <- convert (show @Failure) $ runReaderT fmtAct FmtEnv { .. }
              pure (baseStyle, score)

  logInfoN [i|Using initial style: #{baseStyle} with score of #{baseScore}|]
  baseOptions <- parseOpts execName $ defaultStyleOpts baseStyle varyingOptions allFixedOpts

  let filledOptions | Just (_, resumeOptions) <- maybeResumeOptions = baseOptions `replaceItemsWith` resumeOptions
                    | otherwise = baseOptions

  pure InitializeOptionsResult { .. }
  where
    hardcodedOptsNames = name <$> hardcodedOpts

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
  , formatter :: Formatter
  }

runOptPipeline :: (MonadError String m, MonadLoggerIO m)
               => PipelineOpts -> m BS.ByteString
runOptPipeline PipelineOpts { formatter = formatter@Formatter { .. }, .. } = do
  preparedFiles <- mapM prepareFile $ toList input

  InitializeOptionsResult { .. } <- initializeOptions formatter preparedFiles resumePath forceStrs

  let categoricalVariables = [ IxedVariable dv idx
                             | (Just dv, idx) <- zip (typToDV . value <$> filledOptions) [0..]
                             ]
  let integralVariables = [ IxedVariable dv idx
                          | (Just dv, idx) <- zip (typToIV . value <$> filledOptions) [0..]
                          ]
  let constantOpts = hardcodedOpts formatterInfo <> userForcedOpts
  let fmtEnv = FmtEnv { .. }
  let optEnv = OptEnv { maxSubsetSize = fromMaybe 1 maxSubsetSize, .. }
  let optState = initOptState filledOptions baseScore
  finalOptState <- convert (show @UnexpectedFailure) $ flip runReaderT (fmtEnv, optEnv, taskGroup) $ execStateT (fixGD Nothing 1) optState
  pure $ serializeOptions formatterInfo baseStyle $ constantOpts <> currentOpts finalOptState `subtractMatching` baseOptions
