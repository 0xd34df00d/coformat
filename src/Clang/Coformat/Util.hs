{-# LANGUAGE FlexibleContexts, RankNTypes #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Clang.Coformat.Util where

import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.Async.Pool as A.P
import qualified Control.Monad.Except as E
import qualified Control.Monad.Reader as R
import qualified Data.Text.Lazy as TL
import Control.Monad.Except.CoHas
import Control.Monad.Logger
import Control.Monad.Reader.Has
import Data.Bifunctor
import GHC.Generics
import System.Exit

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

checked :: (MonadError err m, CoHas UnexpectedFailure err, CoHas ExpectedFailure err, MonadIO m)
        => IO (ExitCode, TL.Text, TL.Text) -> m TL.Text
checked act = do
  (ec, stdout, stderr) <- liftIO act
  case ec of ExitSuccess -> pure stdout
             ExitFailure n | n == cfCrashRetCode -> throwError $ FormatterSegfaulted stderr
                           | otherwise -> throwError $ FormatterFailure n stderr
  where
    cfCrashRetCode = -8

update :: Int -> (a -> a) -> [a] -> [a]
update idx f = zipWith z [0..]
  where z idx' val = if idx' == idx then f val else val

forConcurrently' :: (MonadLoggerIO m, MonadError e m)
                 => [a]
                 -> (forall m'. (MonadLoggerIO m', MonadError e m') => a -> m' b)
                 -> m [b]
forConcurrently' lst act = do
  logger <- askLoggerIO
  result <- liftIO $ A.forConcurrently lst $ \elt -> flip runLoggingT logger $ runExceptT $ act elt
  E.liftEither $ sequence result

forConcurrentlyPooled :: (MonadLoggerIO m, MonadError e m, MonadReader r m, Has A.P.TaskGroup r)
                      => [a]
                      -> (forall m'. (MonadLoggerIO m', MonadReader r m', MonadError e m') => a -> m' b)
                      -> m [b]
forConcurrentlyPooled lst act = do
  logger <- askLoggerIO
  tg <- ask
  env <- R.ask
  result <- liftIO $ flip (A.P.mapConcurrently tg) lst $ \elt -> flip runLoggingT logger $ runExceptT $ flip runReaderT env $ act elt
  E.liftEither $ sequence result

convert :: MonadError e' m
        => (e -> e')
        -> ExceptT e m a
        -> m a
convert cvt act = runExceptT act >>= (E.liftEither . first cvt)
