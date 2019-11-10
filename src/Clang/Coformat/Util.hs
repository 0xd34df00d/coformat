{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Clang.Coformat.Util where

import qualified Data.Text.Lazy as TL
import Control.Monad.Except
import Data.String.Interpolate.IsString
import System.Exit

checked :: (MonadError String m, MonadIO m) => IO (ExitCode, TL.Text, TL.Text) -> m TL.Text
checked act = do
  (ec, stdout, stderr) <- liftIO act
  case ec of ExitSuccess -> pure stdout
             ExitFailure n -> throwError [i|clang-format failed with exit code #{n}:\n#{stderr}|]
