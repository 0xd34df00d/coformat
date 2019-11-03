{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Clang.Format.DescrFiller
( fillConfigItemsIO
, FillError
) where

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Control.Monad.Except.CoHas
import Data.Either.Combinators
import Data.Scientific
import Data.Yaml
import GHC.Generics

import Clang.Format.Descr

data FillError
  = YamlParseError ParseException
  | YamlAnalysisError YamlAnalysisError
  deriving (Show, Generic, CoHas ParseException, CoHas YamlAnalysisError)

fillConfigItemsIO :: (MonadIO m, MonadError e m, CoHas ParseException e, CoHas YamlAnalysisError e)
                  => [ConfigItemT 'Supported]
                  -> FilePath
                  -> m [ConfigItemT 'Value]
fillConfigItemsIO supported path = liftIO (decodeFileEither path)
                               >>= liftEither
                               >>= extractMap
                               >>= braceWrappingKludge
                               >>= fillConfigItems supported

data YamlAnalysisError
  = YamlNotAnObject String
  | ValueNotFound T.Text
  | IncompatibleValue T.Text (ConfigTypeT 'Supported) Value
  deriving (Show)

extractMap :: (MonadError e m, CoHas YamlAnalysisError e) => Value -> m Object
extractMap (Object fields) = pure fields
extractMap _ = throwError $ YamlNotAnObject "Top-level value is not an object"

braceWrappingKludge :: (MonadError e m, CoHas YamlAnalysisError e) => Object -> m Object
braceWrappingKludge fields = do
  bwVal <- liftEither $ maybeToRight (ValueNotFound bwField) $ HM.lookup bwField fields
  bwValObj <- case bwVal of
                   Object obj -> pure obj
                   _ -> throwError $ YamlNotAnObject "Brace wrapping value is not an object"
  let bwSubfields = HM.fromList [ (bwField <> "." <> key, val)
                                | (key, val) <- HM.toList bwValObj
                                ]
  pure $ HM.delete bwField fields <> bwSubfields
  where
    bwField = "BraceWrapping"

fillConfigItems :: (MonadError e m, CoHas YamlAnalysisError e)
                => [ConfigItemT 'Supported] -> Object -> m [ConfigItemT 'Value]
fillConfigItems supported fields = mapM fillConfigItem supported
  where
    fillConfigItem ConfigItem { .. } = do
      yamlVal <- case HM.lookup name fields of
                      Just val -> pure val
                      Nothing -> throwError $ ValueNotFound name
      val <- case (typ, yamlVal) of
                  (CTInt _, Number num)
                      | Just int <- toBoundedInteger num -> pure $ CTInt int
                  (CTUnsigned _, Number num)
                      | Just int <- toBoundedInteger num :: Maybe Int
                      , int >= 0 -> pure $ CTUnsigned $ fromIntegral int
                  (CTBool _, Bool b) -> pure $ CTBool b
                  (CTEnum vars _, String s)
                      | s `elem` vars -> pure $ CTEnum vars s
                  _ -> throwError $ IncompatibleValue name typ yamlVal
      pure ConfigItem { name = name, typ = val }
