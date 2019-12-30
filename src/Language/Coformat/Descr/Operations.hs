{-# LANGUAGE DataKinds, GADTs, ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards, LambdaCase, QuasiQuotes, OverloadedStrings #-}

module Language.Coformat.Descr.Operations where

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Control.Monad.Except
import Data.Bifunctor
import Data.Maybe
import Data.String.Interpolate.IsString
import Data.Typeable
import Data.Void
import Text.Read

import Language.Coformat.Descr

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

type ParseableConfigState f = (CTData f Void ~ Void, Show (ConfigTypeT f))

parseConfigValue :: (MonadError String m, ParseableConfigState f) => ConfigItemT f -> String -> m (ConfigItemT 'Value)
parseConfigValue cfg str = liftEither $ (\parsed -> ConfigItem { name = name cfg, value = parsed }) <$> eitherParsed
  where
    eitherParsed = case value cfg of
                        CTInt _ -> CTInt <$> readEither'
                        CTUnsigned _ -> CTUnsigned <$> readEither'
                        CTBool _ -> CTBool <$> readEither'
                        CTString val -> absurd val
                        CTStringVec val -> absurd val
                        CTRawStringFormats val -> absurd val
                        CTIncludeCats val -> absurd val
                        CTEnum variants _ | var `elem` variants -> pure $ CTEnum variants var
                                          | otherwise -> throwError [i|Unsupported option `#{var}`, supported ones are `#{T.intercalate "`, `" variants}`|]
                            where var = T.pack str
    readEither' :: forall a. (Typeable a, Read a) => Either String a
    readEither' = first (\err -> [i|Error parsing #{str} as #{typeRep (Proxy :: Proxy a)}: #{err}, expected type: #{value cfg}|]) $ readEither str
