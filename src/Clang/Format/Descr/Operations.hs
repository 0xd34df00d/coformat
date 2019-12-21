{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards, LambdaCase, QuasiQuotes, OverloadedStrings #-}

module Clang.Format.Descr.Operations where

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Control.Monad.Except
import Data.Maybe
import Data.String.Interpolate.IsString
import Data.Void
import Text.Read

import Clang.Format.Descr

filterParsedItems :: [ConfigItemT 'Parsed] -> [ConfigItemT 'Supported]
filterParsedItems = mapMaybe $ \ConfigItem { .. } -> ConfigItem name <$> filterType value
  where
    filterType = \case
                    CTInt () -> Just $ CTInt ()
                    CTUnsigned () -> Just $ CTUnsigned ()
                    CTBool () -> Just $ CTBool ()
                    CTString () -> Nothing
                    CTStringVec () -> Nothing
                    CTRawStringFormats () -> Nothing
                    CTIncludeCats () -> Nothing
                    CTEnum vars () -> Just $ CTEnum vars ()

replaceItemsWith :: [ConfigItemT 'Value] -> [ConfigItemT 'Value] -> [ConfigItemT 'Value]
replaceItemsWith l1 l2 = M.elems $ toMap l2 <> toMap l1
  where
    toMap lst = M.fromList [ (name item, item) | item <- lst ]

subtractMatching :: [ConfigItemT 'Value] -> [ConfigItemT 'Value] -> [ConfigItemT 'Value]
subtractMatching minuend subtrahend = filter f minuend
  where
    f ConfigItem { .. } = (/= Just value) $ HM.lookup name subMap
    subMap = HM.fromList [ (name, value) | ConfigItem { .. } <- subtrahend]

parseConfigValue :: MonadError String m => (CTData f Void ~ Void) => ConfigItemT f -> String -> m (ConfigItemT 'Value)
parseConfigValue cfg str = liftEither $ (\parsed -> ConfigItem { name = name cfg, value = parsed }) <$> eitherParsed
  where
    eitherParsed = case value cfg of
                        CTInt _ -> CTInt <$> readEither str
                        CTUnsigned _ -> CTUnsigned <$> readEither str
                        CTBool _ -> CTBool <$> readEither str
                        CTString val -> absurd val
                        CTStringVec val -> absurd val
                        CTRawStringFormats val -> absurd val
                        CTIncludeCats val -> absurd val
                        CTEnum variants _ -> do
                          var <- readEither str
                          unless (var `elem` variants) $
                            throwError [i|Unsupported option `${var}`, supported ones are `#{T.intercalate "`, `" variants}`|]
                          pure $ CTEnum variants var
