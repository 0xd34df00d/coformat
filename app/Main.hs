{-# LANGUAGE FlexibleContexts, TypeApplications #-}
{-# LANGUAGE DataKinds, RankNTypes, GADTs #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, QuasiQuotes #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TL
import Control.Monad.Except
import Control.Monad.Logger
import Data.Bifunctor
import Data.Foldable
import Data.Maybe
import Data.String.Interpolate.IsString
import System.Command.QQ

import Clang.Coformat.Optimization
import Clang.Coformat.Util
import Clang.Format.Descr
import Clang.Format.DescrParser
import Clang.Format.YamlParser

liftEither' :: (MonadError String m, Show e) => String -> Either e a -> m a
liftEither' context = liftEither . first ((context <>) . show)

convert :: (MonadError String m, MonadIO m, Show e)
        => (e -> String)
        -> (forall m'. (MonadError e m', MonadIO m') => m' a)
        -> m a
convert cvt act = runExceptT act >>= (liftEither . first cvt)

parseOptsDescription :: (MonadError String m, MonadIO m) => FilePath -> m ([T.Text], [ConfigItemT 'Supported])
parseOptsDescription path = do
  parseResult <- liftIO $ parseFile path
  supportedOptions <- filterParsedItems <$> liftEither' "Unable to parse the file: " parseResult
  baseStyles <- case find ((== bosKey) . name) supportedOptions of
                     Nothing -> throwError "No `BasedOnStyle` option"
                     Just stys -> pure stys
  let varyingOptions = filter ((/= bosKey) . name) supportedOptions
  case typ baseStyles of
       CTEnum { .. } -> pure (variants, varyingOptions)
       _ -> throwError [i|Unknown type for the `BaseStyles` option: #{typ baseStyles}|]
  where
    bosKey = "BasedOnStyle"

data UserForcedOpts = UserForcedOpts
  { tabWidth :: Maybe Int
  , useTab :: Maybe Bool
  } deriving (Eq, Show)

toConfigItems :: UserForcedOpts -> [ConfigItemT 'Value]
toConfigItems UserForcedOpts { .. } =
  catMaybes [ unpackInt "TabWidth" tabWidth
            , unpackEnum "UseTab" $ (\b -> if b then "Always" else "Never") <$> useTab
            ]
  where
    unpackInt _ Nothing = Nothing
    unpackInt label (Just val) = Just ConfigItem { name = label, typ = CTInt val }
    unpackEnum _ Nothing = Nothing
    unpackEnum label (Just val) = Just ConfigItem { name = label, typ = CTEnum [val] val }

data Options = Options
  { userFocedOpts :: UserForcedOpts
  , inputFiles :: [String]
  } deriving (Eq, Show)

doWork :: (MonadError String m, MonadLoggerIO m) => m ()
doWork = do
  (baseStyles, varyingOptions) <- parseOptsDescription "data/ClangFormatStyleOptions-9.html"
  baseStyle <- chooseBaseStyle (UserForcedOpts (Just 4) (Just True)) baseStyles ["data/core.cpp", "data/core.h"]
  stdout <- checked [sh|clang-format --style=#{baseStyle} --dump-config|]
  filledOptions <- convert (show @FillError) $ fillConfigItems varyingOptions $ BSL.toStrict $ TL.encodeUtf8 stdout
  liftIO $ mapM_ print filledOptions

main :: IO ()
main = do
  res <- runStderrLoggingT $ runExceptT doWork
  case res of
       Left err -> putStrLn err
       Right () -> putStrLn "done"
