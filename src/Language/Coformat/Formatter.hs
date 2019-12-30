{-# LANGUAGE DataKinds, GADTs, RankNTypes, TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Coformat.Formatter where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import Control.Monad.Except.CoHas
import System.Command hiding(cmd)
import System.Exit

import Language.Coformat.Descr
import Language.Coformat.Formatter.Failure
import Language.Coformat.Util

data OptsDescription stage = OptsDescription
  { knownOpts :: [ConfigItemT stage]
  , baseStyles :: [T.Text]
  }

data OptsSource opts
  = StaticOpts opts
  | OptsFromFile FilePath (LBS.ByteString -> Either String opts)
  | OptsFromCmd CmdArgs (BS.ByteString -> Either String opts)

parseOpts :: (MonadIO m, MonadError String m) => String -> OptsSource opts -> m opts
parseOpts _     (StaticOpts d) = pure d
parseOpts _     (OptsFromFile path parser) = parser <$> liftIO (LBS.readFile path) >>= liftEither
parseOpts exec  (OptsFromCmd args parser) = convert (show @Failure) (runCommand exec args) >>= liftEither . parser

data FormatterInfo = FormatterInfo
  { execName :: String

  , formatterOpts :: OptsSource (OptsDescription 'Supported)
  , hardcodedOpts :: [ConfigItemT 'Value]
  , defaultStyleOpts :: T.Text -> [ConfigItemT 'Supported] -> [ConfigItemT 'Value] -> OptsSource [ConfigItemT 'Value]

  , formatFile :: T.Text -> [ConfigItemT 'Value] -> FilePath -> CmdArgs

  , serializeOptions :: T.Text -> [ConfigItemT 'Value] -> BS.ByteString
  }

newtype CmdArgs = CmdArgs
  { args :: [BS.ByteString]
  } deriving (Show)

runCommand :: (MonadError err m, CoHas UnexpectedFailure err, CoHas ExpectedFailure err, MonadIO m)
           => String -> CmdArgs -> m BS.ByteString
runCommand exec (CmdArgs args) = do
  (ec, stdout, stderr) <- liftIO $ command [] exec $ BS.unpack <$> args
  case ec of ExitSuccess -> pure $ BS.pack $ fromStdout stdout
             ExitFailure n | n == cfCrashRetCode -> throwError $ FormatterSegfaulted $ T.pack $ fromStderr stderr
                           | otherwise -> throwError $ FormatterFailure n $ T.pack $ fromStderr stderr
  where
    cfCrashRetCode = -8

data Formatter where
  Formatter :: forall resumeObj.
               { formatterInfo :: FormatterInfo
               , parseResumeObject :: BS.ByteString -> Either String resumeObj
               , parseResumeOptions :: [ConfigItemT 'Supported] -> resumeObj -> Either String (T.Text, [ConfigItemT 'Value])
               } -> Formatter
