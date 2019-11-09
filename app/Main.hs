{-# LANGUAGE FlexibleContexts, TypeApplications #-}
{-# LANGUAGE DataKinds, RankNTypes #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, QuasiQuotes #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Control.Concurrent.Async
import Control.Monad.Except
import Data.Bifunctor
import Data.List
import Data.Ord
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

checkExit :: MonadError String m => String -> ExitCode -> TL.Text -> m ()
checkExit _ ExitSuccess _ = pure ()
checkExit program (ExitFailure n) stderr = throwError [i|#{program} failed with exit code #{n}:\n#{stderr}|]

chooseBaseStyle :: (MonadError String m, MonadIO m) => [T.Text] -> [String] -> m T.Text
chooseBaseStyle baseStyles files = do
  estimates <- liftIO $ forConcurrently ((,) <$> baseStyles <*> files) $ \(sty, file) -> runExceptT $ do
    (ec, stdout, stderr) <- liftIO [sh|clang-format --style="{BasedOnStyle: #{sty}, TabWidth: 4, UseTab: Always}" #{file}|]
    checkExit "clang-format" ec stderr
    source <- liftIO $ readFile file
    let dist = levenshteinDistance defaultEditCosts source $ TL.unpack stdout
    liftIO $ putStrLn [i|Initial guess for #{sty} at #{file}: #{dist}|]
    pure (sty, dist)
  sty2dists <- liftEither $ sequence estimates
  let accumulated = HM.toList $ HM.fromListWith (+) sty2dists
  liftIO $ putStrLn "==="
  liftIO $ forM_ accumulated $ \(sty, acc) -> putStrLn [i|Initial accumulated guess for #{sty}: #{acc}|]
  pure $ fst $ minimumBy (comparing snd) accumulated

doWork :: (MonadError String m, MonadIO m) => m ()
doWork = do
  (baseStyles, varyingOptions) <- parseOptsDescription "data/ClangFormatStyleOptions-9.html"
  baseStyle <- chooseBaseStyle baseStyles ["data/core.cpp", "data/core.h"]
  (ec, stdout, stderr) <- liftIO [sh|clang-format --style=#{baseStyle} --dump-config|]
  filledOptions <- convert (show @FillError) $ fillConfigItems varyingOptions $ BSL.toStrict $ TL.encodeUtf8 stdout
  liftIO $ mapM_ print filledOptions

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
