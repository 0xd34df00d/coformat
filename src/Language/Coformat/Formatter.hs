{-# LANGUAGE DataKinds, ConstraintKinds #-}
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

data OptsDescription = OptsDescription
  { knownOpts :: [ConfigItemT 'Supported]
  , baseStyles :: [T.Text]
  } deriving (Show)

data OptsSource
  = StaticOpts OptsDescription
  | OptsFromFile FilePath (BS.ByteString -> OptsDescription)

data FormatterInfo = FormatterInfo
  { executableName :: String
  , formatterOpts :: OptsSource
  }

data Cmd = Cmd
  { exec :: String
  , args :: [String]
  } deriving (Show)

data Formatter = Formatter
  { formatterInfo :: FormatterInfo
  , formatFile :: T.Text -> [ConfigItemT 'Value] -> FilePath -> Cmd
  }


type FormatterMonad err m = (MonadError err m, CoHas UnexpectedFailure err, CoHas ExpectedFailure err, MonadIO m)

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
