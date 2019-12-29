{-# LANGUAGE DataKinds, ConstraintKinds, GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Coformat.Formatter where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import Control.Monad.Except.CoHas
import GHC.Generics
import System.Command
import System.Exit

import Clang.Format.Descr

data OptsDescription = OptsDescription
  { knownOpts :: [ConfigItemT 'Supported]
  , baseStyles :: [T.Text]
  } deriving (Show)

data OptsSource
  = StaticOpts OptsDescription
  | OptsFromFile FilePath (LBS.ByteString -> Either String OptsDescription)

parseOpts :: MonadIO m => OptsSource -> m (Either String OptsDescription)
parseOpts (StaticOpts d) = pure $ Right d
parseOpts (OptsFromFile path parser) = parser <$> liftIO (LBS.readFile path)

data FormatterInfo = FormatterInfo
  { executableName :: String
  , formatterOpts :: OptsSource
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

class Formatter a where
  formatterInfo :: proxy a -> FormatterInfo

data SomeFormatter where
  SomeFormatter :: Formatter a => proxy a -> SomeFormatter

someFormatterInfo :: SomeFormatter -> FormatterInfo
someFormatterInfo (SomeFormatter fmt) = formatterInfo fmt

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