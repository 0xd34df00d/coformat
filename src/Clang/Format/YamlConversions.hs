{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Clang.Format.YamlConversions
( fillConfigItems
, FillError

, formatClangFormat
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Control.Monad.Except.CoHas
import Data.Either.Combinators
import Data.Scientific
import Data.Yaml
import Data.Yaml.Pretty
import GHC.Generics

import Clang.Format.Descr
import Clang.Coformat.StyOpts

data FillError
  = YamlParseError ParseException
  | YamlAnalysisError YamlAnalysisError
  deriving (Show, Generic, CoHas ParseException, CoHas YamlAnalysisError)

fillConfigItems :: (MonadError e m, CoHas ParseException e, CoHas YamlAnalysisError e)
                => [ConfigItemT 'Supported] -> BS.ByteString -> m [ConfigItemT 'Value]
fillConfigItems supported yamlContents = liftEither (decodeEither' yamlContents)
                                     >>= extractMap
                                     >>= braceWrappingKludge
                                     >>= fillConfigItemsFromObj supported

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

fillConfigItemsFromObj :: (MonadError e m, CoHas YamlAnalysisError e)
                       => [ConfigItemT 'Supported] -> Object -> m [ConfigItemT 'Value]
fillConfigItemsFromObj supported fields = mapM fillConfigItem supported
  where
    fillConfigItem ConfigItem { .. } = do
      yamlVal <- case HM.lookup nameConcatted fields of
                      Just val -> pure val
                      Nothing -> throwError $ ValueNotFound nameConcatted
      val <- case (typ, yamlVal) of
                  (CTInt _, Number num)
                      | Just int <- toBoundedInteger num -> pure $ CTInt int
                  (CTUnsigned _, Number num)
                      | Just int <- toBoundedInteger num :: Maybe Int
                      , int >= 0 -> pure $ CTUnsigned $ fromIntegral int
                  (CTBool _, Bool b) -> pure $ CTBool b
                  (CTEnum vars _, String s)
                      | s `elem` vars -> pure $ CTEnum vars s
                  _ -> throwError $ IncompatibleValue nameConcatted typ yamlVal
      pure ConfigItem { name = name, typ = val }
      where
        nameConcatted = T.intercalate "." name

formatClangFormat :: StyOpts -> BS.ByteString
formatClangFormat = encodePretty $ setConfCompare compare defConfig
