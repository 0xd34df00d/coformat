{-# LANGUAGE DataKinds, RankNTypes, ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Language.Coformat.Formatter where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Monad.Except.CoHas
import Control.Monad.IO.Class
import GHC.Generics

import Clang.Format.Descr

data FormatterInfo = FormatterInfo
  { executableName :: String
  , knownOptions :: forall m. MonadIO m => m [ConfigItemT 'Supported]
  , baseStyles :: [T.Text]
  }

type FormatterMonad err m = (MonadError err m, CoHas UnexpectedFailure err, CoHas ExpectedFailure err, MonadIO m)

data Formatter = Formatter
  { formatterInfo :: FormatterInfo
  , formatFile :: forall err m. FormatterMonad err m => T.Text -> [ConfigItemT 'Value] -> FilePath -> m BS.ByteString
  }

data ExpectedFailure = FormatterSegfaulted TL.Text   -- kek
  deriving (Eq, Show)

data UnexpectedFailure = FormatterFailure
  { errorCode :: Int
  , errorOutput :: TL.Text
  } deriving (Eq, Show)

data Failure = ExpectedFailure ExpectedFailure
             | UnexpectedFailure UnexpectedFailure
             deriving (Eq, Show, Generic, CoHas ExpectedFailure, CoHas UnexpectedFailure)

failuresAreUnexpected :: Failure -> UnexpectedFailure
failuresAreUnexpected (UnexpectedFailure err) = err
failuresAreUnexpected (ExpectedFailure (FormatterSegfaulted out)) = FormatterFailure 0 out
