{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Clang.Format.DescrFiller
( fillConfigItemsIO
) where

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Control.Monad.Except
import Control.Monad.Except.CoHas
import Data.Bifunctor
import Data.Scientific
import Data.Yaml
import GHC.Generics

import Clang.Format.Descr

data FillError
  = YamlParseError ParseException
  | YamlAnalysisError YamlAnalysisError
  deriving (Generic, CoHas ParseException, CoHas YamlAnalysisError)

liftEitherCoHas :: (MonadError sum m, CoHas e sum) => Either e a -> m a
liftEitherCoHas = liftEither . first inject

fillConfigItemsIO :: (MonadIO m, MonadError e m, CoHas ParseException e, CoHas YamlAnalysisError e)
                  => [ConfigItemT 'Supported]
                  -> FilePath
                  -> m [ConfigItemT 'WithDefault]
fillConfigItemsIO supported path = liftIO (decodeFileEither path) >>= liftEitherCoHas >>= fillConfigItems supported

data YamlAnalysisError
  = YamlNotAnObject
  | ValueNotFound T.Text
  | IncompatibleValue (ConfigTypeT 'Supported) Value
  deriving (Show)

fillConfigItems :: (MonadError e m, CoHas YamlAnalysisError e)
                => [ConfigItemT 'Supported] -> Value -> m [ConfigItemT 'WithDefault]
fillConfigItems supported (Object fields) = mapM fillConfigItem supported
  where
    fillConfigItem ConfigItem { .. } = do
      yamlVal <- case HM.lookup name fields of
                      Just val -> pure val
                      Nothing -> throwError $ inject $ ValueNotFound name
      val <- case (typ, yamlVal) of
                  (CTInt _, Number num)
                      | Just int <- toBoundedInteger num -> pure $ CTInt int
                  (CTUnsigned _, Number num)
                      | Just int <- toBoundedInteger num :: Maybe Int
                      , int >= 0 -> pure $ CTUnsigned $ fromIntegral int
                  (CTBool _, Bool b) -> pure $ CTBool b
                  (CTEnum vars _, String s)
                      | s `elem` vars -> pure $ CTEnum vars s
                  _ -> throwError $ inject $ IncompatibleValue typ yamlVal
      pure ConfigItem { name = name, typ = val }
fillConfigItems _ _ = throwError $ inject YamlNotAnObject
