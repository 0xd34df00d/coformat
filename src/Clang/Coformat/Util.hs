{-# LANGUAGE FlexibleContexts, RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Clang.Coformat.Util where

import qualified Data.Text.Lazy as TL
import Control.Concurrent.Async
import Control.Monad.Except
import Control.Monad.Logger
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
  result <- liftIO $ forConcurrently lst $ \elt -> flip runLoggingT logger $ runExceptT $ act elt
  liftEither $ sequence result
