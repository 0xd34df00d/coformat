{-# LANGUAGE FlexibleContexts, RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Clang.Coformat.Util where

import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.Async.Pool as A.P
import qualified Control.Monad.Except as E
import qualified Data.Text.Lazy as TL
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader.Has
import Data.Bifunctor
import Data.String.Interpolate.IsString
import System.Exit

checked :: (MonadError String m, MonadIO m) => IO (ExitCode, TL.Text, TL.Text) -> m TL.Text
checked act = do
  (ec, stdout, stderr) <- liftIO act
  case ec of ExitSuccess -> pure stdout
             ExitFailure n -> throwError [i|clang-format failed with exit code #{n}:\n#{stderr}|]

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
  liftEither $ sequence result

forConcurrentlyPooled :: (MonadLoggerIO m, MonadError e m, MonadReader r m, Has A.P.TaskGroup r)
                      => [a]
                      -> (forall m'. (MonadLoggerIO m', MonadError e m') => a -> m' b)
                      -> m [b]
forConcurrentlyPooled lst act = do
  logger <- askLoggerIO
  tg <- ask
  result <- liftIO $ flip (A.P.mapConcurrently tg) lst $ \elt -> flip runLoggingT logger $ runExceptT $ act elt
  liftEither $ sequence result

convert :: MonadError e' m
        => (e -> e')
        -> ExceptT e m a
        -> m a
convert cvt act = runExceptT act >>= (E.liftEither . first cvt)
