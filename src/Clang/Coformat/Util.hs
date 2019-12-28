{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Clang.Coformat.Util where

import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.Async.Pool as A.P
import qualified Control.Monad.Except as E
import qualified Control.Monad.Reader as R
import Control.Monad.Except.CoHas
import Control.Monad.Logger
import Control.Monad.Reader.Has
import Data.Bifunctor
import Numeric.Natural

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

subsetsN :: Natural -> [a] -> [[a]]
subsetsN _ [] = []
subsetsN 1 xs = pure <$> xs
subsetsN n (x:xs) = ((x:) <$> subsetsN (n - 1) xs) <> subsetsN n xs
