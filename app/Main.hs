{-# LANGUAGE FlexibleContexts, TypeApplications #-}
{-# LANGUAGE DataKinds, RankNTypes #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Control.Monad.Except
import Data.Bifunctor
import Data.List
import Data.Yaml

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
       _ -> throwError $ "Unknown type for the `BaseStyles` option: " <> show (typ baseStyles)
  where
    bosKey = "BasedOnStyle"

doWork :: (MonadError String m, MonadIO m) => m ()
doWork = do
  (baseStyles, varyingOptions) <- parseOptsDescription "data/ClangFormatStyleOptions-9.html"
  liftIO $ print baseStyles
  filledOptions <- convert (show @FillError) $ fillConfigItemsIO varyingOptions "sample.yaml"
  liftIO $ mapM_ print filledOptions
  pure ()

testYaml :: IO ()
testYaml = do
  Right (_, Object res) <- decodeFileWithWarnings @Value "sample.yaml"
  mapM_ print $ sortOn fst $ HM.toList res

main :: IO ()
main = do
  res <- runExceptT doWork
  case res of
       Left err -> putStrLn err
       Right () -> putStrLn "done"
