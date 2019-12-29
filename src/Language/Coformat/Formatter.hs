{-# LANGUAGE DataKinds, ConstraintKinds, GADTs, RankNTypes, TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Coformat.Formatter where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import Control.Monad.Except.CoHas
import GHC.Generics
import System.Command hiding(cmd)
import System.Exit

import Clang.Coformat.Util
import Clang.Format.Descr

data OptsDescription stage = OptsDescription
  { knownOpts :: [ConfigItemT stage]
  , baseStyles :: [T.Text]
  }

data OptsSource opts
  = StaticOpts opts
  | OptsFromFile FilePath (LBS.ByteString -> Either String opts)
  | OptsFromCmd Cmd (BS.ByteString -> Either String opts)

parseOpts :: MonadIO m => OptsSource opts -> m (Either String opts)
parseOpts (StaticOpts d) = pure $ Right d
parseOpts (OptsFromFile path parser) = parser <$> liftIO (LBS.readFile path)
parseOpts (OptsFromCmd cmd parser) = runExceptT $ convert (show @Failure) (runCommand cmd) >>= liftEither . parser

data FormatterInfo = FormatterInfo
  { executableName :: String

  , formatterOpts :: OptsSource (OptsDescription 'Supported)
  , hardcodedOpts :: [ConfigItemT 'Value]

  , formatFile :: T.Text -> [ConfigItemT 'Value] -> FilePath -> Cmd
  }

data Cmd = Cmd
  { exec :: String
  , args :: [BS.ByteString]
  } deriving (Show)

runCommand :: FormatterMonad err m => Cmd -> m BS.ByteString
runCommand Cmd { .. } = do
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
               , parseResumeOptions :: [ConfigItemT 'Supported] -> resumeObj -> Either String [ConfigItemT 'Value]
               } -> Formatter

type FormatterMonad err m = (MonadError err m, CoHas UnexpectedFailure err, CoHas ExpectedFailure err, MonadIO m)

data ExpectedFailure = FormatterSegfaulted T.Text   -- kek
  deriving (Eq, Show)

data UnexpectedFailure = FormatterFailure
  { errorCode :: Int
  , errorOutput :: T.Text
  } deriving (Eq, Show)

data Failure = ExpectedFailure ExpectedFailure
             | UnexpectedFailure UnexpectedFailure
             deriving (Eq, Show, Generic, CoHas ExpectedFailure, CoHas UnexpectedFailure)

failuresAreUnexpected :: Failure -> UnexpectedFailure
failuresAreUnexpected (UnexpectedFailure err) = err
failuresAreUnexpected (ExpectedFailure (FormatterSegfaulted out)) = FormatterFailure 0 out
