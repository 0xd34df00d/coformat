{-# LANGUAGE FlexibleContexts, TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.HashMap.Strict as HM
import Control.Monad.Except
import Data.Bifunctor
import Data.List
import Data.Yaml

import Clang.Format.DescrParser
import Clang.Format.DescrFiller
import Clang.Format.Descr

liftEither' :: (MonadError String m, Show e) => String -> Either e a -> m a
liftEither' context = liftEither . first ((context <>) . show)

convert :: (MonadError String m, MonadIO m, Show e)
        => (e -> String)
        -> (forall m'. (MonadError e m', MonadIO m') => m' a)
        -> m a
convert cvt act = runExceptT act >>= (liftEither . first cvt)

doWork :: (MonadError String m, MonadIO m) => m ()
doWork = do
  parseResult <- liftIO $ parseFile "data/ClangFormatStyleOptions-9.html"
  supportedOptions <- filterParsedItems <$> liftEither' "Unable to parse the file: " parseResult
  baseStyles <- case find ((== bosKey) . name) supportedOptions of
                     Nothing -> throwError "No `BasedOnStyle` option"
                     Just stys -> pure stys
  let varyingOptions = filter ((/= bosKey) . name) supportedOptions
  filledOptions <- convert (show @FillError) $ fillConfigItemsIO varyingOptions "sample.yaml"
  pure ()
  where
    bosKey = "BasedOnStyle"

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
