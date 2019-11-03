{-# LANGUAGE FlexibleContexts, TypeApplications #-}
{-# LANGUAGE DataKinds, RankNTypes #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, QuasiQuotes #-}

module Main where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Concurrent.Async
import Control.Monad.Except
import Data.Bifunctor
import Data.List
import Data.String.Interpolate
import Data.Yaml
import System.Command.QQ
import System.Exit
import Text.EditDistance

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

doWork :: (MonadError String m, MonadIO m) => m ()
doWork = do
  (baseStyles, varyingOptions) <- parseOptsDescription "data/ClangFormatStyleOptions-9.html"
  liftIO $ forConcurrently_ baseStyles $ \sty -> do
    (ec, stdout, stderr) <- [sh|clang-format --style="{BasedOnStyle: #{sty}}" data/core.cpp|]
    case ec of ExitSuccess -> pure ()
               ExitFailure n -> putStrLn [i|clang-format failed with exit code #{n}:\n#{stderr}|]
    source <- readFile "data/core.cpp"
    putStrLn [i|#{sty}: #{levenshteinDistance defaultEditCosts source $ TL.unpack stdout}|]

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
