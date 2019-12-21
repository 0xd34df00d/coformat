{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings, LambdaCase #-}

module Clang.Format.YamlConversions
( fillConfigItems
, FillError

, YamlConfigType(..)
, preprocessYaml

, collectConfigItems

, formatClangFormat
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Control.Monad.Except.CoHas
import Data.Either
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

data YamlConfigType = StyleDump | PartialConfig

preprocessYaml :: (MonadError e m, CoHas ParseException e, CoHas YamlAnalysisError e, CoHas ValueNotFound e)
               => YamlConfigType -> BS.ByteString -> m Object
preprocessYaml configType yamlContents = liftEither (decodeEither' yamlContents)
                                     >>= extractMap
                                     >>= bwWithFailure configType
  where
    bwWithFailure StyleDump = braceWrappingKludge >=> liftEither
    bwWithFailure PartialConfig = \obj -> braceWrappingKludge obj
                                      >>= \case Left _ -> pure obj
                                                Right obj' -> pure obj'

-- Errors out in case of missing items (note 'ValueNotFound' in the constraint).
fillConfigItems :: (MonadError e m, CoHas ParseException e, CoHas YamlAnalysisError e, CoHas ValueNotFound e)
                => [ConfigItemT 'Supported] -> BS.ByteString -> m [ConfigItemT 'Value]
fillConfigItems supported yamlContents = preprocessYaml StyleDump yamlContents
                                     >>= fillConfigItemsFromObj supported
                                     >>= liftEither . sequence

-- Drops missing items.
collectConfigItems :: (MonadError e m, CoHas ParseException e, CoHas YamlAnalysisError e)
                   => [ConfigItemT 'Supported] -> Object -> m [ConfigItemT 'Value]
collectConfigItems supported yamlObject = rights <$> fillConfigItemsFromObj supported yamlObject

data YamlAnalysisError
  = YamlNotAnObject String
  | IncompatibleValue T.Text (ConfigTypeT 'Supported) Value
  deriving (Show)

newtype ValueNotFound = ValueNotFound { missingValueName :: T.Text } deriving (Show)

extractMap :: (MonadError e m, CoHas YamlAnalysisError e) => Value -> m Object
extractMap (Object fields) = pure fields
extractMap _ = throwError $ YamlNotAnObject "Top-level value is not an object"

braceWrappingKludge :: (MonadError e m, CoHas YamlAnalysisError e) => Object -> m (Either ValueNotFound Object)
braceWrappingKludge fields
  | Nothing <- maybeBWVal = pure $ Left $ ValueNotFound bwField
  | Just (Object obj) <- maybeBWVal = let bwSubfields = HM.fromList [ (bwField <> "." <> key, val)
                                                                    | (key, val) <- HM.toList obj
                                                                    ]
                                      in pure $ Right $ HM.delete bwField fields <> bwSubfields
  | otherwise = throwError $ YamlNotAnObject "Brace wrapping value is not an object"
  where
    bwField = "BraceWrapping"
    maybeBWVal = HM.lookup bwField fields

fillConfigItemsFromObj :: (MonadError e m, CoHas YamlAnalysisError e)
                       => [ConfigItemT 'Supported] -> Object -> m [Either ValueNotFound (ConfigItemT 'Value)]
fillConfigItemsFromObj supported fields = mapM fillConfigItem supported
  where
    fillConfigItem ConfigItem { .. }
      | Nothing <- maybeYamlVal = pure $ Left $ ValueNotFound nameConcatted
      | Just yamlVal <- maybeYamlVal = do
        value' <- case (value, yamlVal) of
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
                       _ -> throwError $ IncompatibleValue nameConcatted value yamlVal
        pure $ Right $ ConfigItem { name = name, value = value' }
      where
        nameConcatted = T.intercalate "." name
        maybeYamlVal = HM.lookup nameConcatted fields

formatClangFormat :: StyOpts -> BS.ByteString
formatClangFormat = encodePretty $ setConfCompare compare defConfig

boolAsEnumVar :: Bool -> T.Text
boolAsEnumVar True = "Yes"
boolAsEnumVar False = "No"
