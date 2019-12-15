{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Clang.Format.YamlConversions
( fillConfigItems
, FillError

, preprocessYaml

, formatClangFormat
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Control.Monad.Except.CoHas
import Data.Scientific
import Data.Yaml
import Data.Yaml.Pretty
import GHC.Generics

import Clang.Format.Descr
import Clang.Coformat.StyOpts

data FillError
  = YamlParseError ParseException
  | YamlAnalysisError YamlAnalysisError
  | YamlValueNotFound ValueNotFound
  deriving (Show, Generic, CoHas ParseException, CoHas YamlAnalysisError, CoHas ValueNotFound)

preprocessYaml :: (MonadError e m, CoHas ParseException e, CoHas YamlAnalysisError e, CoHas ValueNotFound e)
               => BS.ByteString -> m Object
preprocessYaml yamlContents = liftEither (decodeEither' yamlContents)
                          >>= extractMap
                          >>= braceWrappingKludge

fillConfigItems :: (MonadError e m, CoHas ParseException e, CoHas YamlAnalysisError e, CoHas ValueNotFound e)
                => [ConfigItemT 'Supported] -> BS.ByteString -> m [ConfigItemT 'Value]
fillConfigItems supported yamlContents = preprocessYaml yamlContents
                                     >>= fillConfigItemsFromObj supported
                                     >>= liftEither . sequence

data YamlAnalysisError
  = YamlNotAnObject String
  | IncompatibleValue T.Text (ConfigTypeT 'Supported) Value
  deriving (Show)

newtype ValueNotFound = ValueNotFound { missingValueName :: T.Text } deriving (Show)

extractMap :: (MonadError e m, CoHas YamlAnalysisError e) => Value -> m Object
extractMap (Object fields) = pure fields
extractMap _ = throwError $ YamlNotAnObject "Top-level value is not an object"

braceWrappingKludge :: (MonadError e m, CoHas YamlAnalysisError e, CoHas ValueNotFound e) => Object -> m Object
braceWrappingKludge fields = do
  bwVal <- liftMaybe (ValueNotFound bwField) $ HM.lookup bwField fields
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
                       => [ConfigItemT 'Supported] -> Object -> m [Either ValueNotFound (ConfigItemT 'Value)]
fillConfigItemsFromObj supported fields = mapM fillConfigItem supported
  where
    fillConfigItem ConfigItem { .. }
      | Nothing <- maybeYamlVal = pure $ Left $ ValueNotFound nameConcatted
      | Just yamlVal <- maybeYamlVal = do
        val <- case (typ, yamlVal) of
                    (CTInt _, Number num)
                        | Just int <- toBoundedInteger num -> pure $ CTInt int
                    (CTUnsigned _, Number num)
                        | Just int <- toBoundedInteger num :: Maybe Int
                        , int >= 0 -> pure $ CTUnsigned $ fromIntegral int
                    (CTBool _, Bool b) -> pure $ CTBool b
                    (CTEnum vars _, String s)
                        | s `elem` vars -> pure $ CTEnum vars s
                    (CTEnum vars _, Bool b)
                        | boolAsEnumVar b `elem` vars -> pure $ CTEnum vars $ boolAsEnumVar b
                    _ -> throwError $ IncompatibleValue nameConcatted typ yamlVal
        pure $ Right $ ConfigItem { name = name, typ = val }
      where
        nameConcatted = T.intercalate "." name
        maybeYamlVal = HM.lookup nameConcatted fields

formatClangFormat :: StyOpts -> BS.ByteString
formatClangFormat = encodePretty $ setConfCompare compare defConfig

boolAsEnumVar :: Bool -> T.Text
boolAsEnumVar True = "Yes"
boolAsEnumVar False = "No"
