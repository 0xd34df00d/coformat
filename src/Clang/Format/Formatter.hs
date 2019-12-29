{-# LANGUAGE GADTs, TypeApplications, DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards, QuasiQuotes, OverloadedStrings #-}

module Clang.Format.Formatter(clangFormatter) where

import qualified Control.Monad.Except.CoHas as EC
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict as HM
import Control.Lens
import Control.Monad.Except
import Data.Aeson.Lens
import Data.Bifunctor
import Data.List
import Data.String.Interpolate

import Language.Coformat.Formatter
import Clang.Coformat.StyOpts
import Clang.Coformat.Util
import Clang.Format.Descr
import Clang.Format.Descr.Operations
import Clang.Format.DescrParser
import Clang.Format.YamlConversions

clangFormatter :: Formatter
clangFormatter = Formatter { .. }
  where
    formatterInfo = FormatterInfo { .. }
      where
        execName = "clang-format"
        formatterOpts = OptsFromFile "data/ClangFormatStyleOptions-9.html" parseOptsDescription
        hardcodedOpts = [ ConfigItem { name = ["Language"], value = CTEnum ["Cpp"] "Cpp" }
                        , ConfigItem { name = ["BreakBeforeBraces"], value = CTEnum ["Custom"] "Custom" }
                        , ConfigItem { name = ["DisableFormat"], value = CTBool False }
                        , ConfigItem { name = ["SortIncludes"], value = CTBool False }
                        ]
        defaultStyleOpts sty supported allFixedOpts = OptsFromCmd (CmdArgs args) parser
          where
            args = [ "--style=" <> formattedBaseSty, "--dump-config" ]
            parser = convert (show @FillError) . fillConfigItems supported
            formattedBaseSty = formatStyArg $ StyOpts { basedOnStyle = sty, additionalOpts = allFixedOpts }

        formatFile baseSty opts path = CmdArgs { args = [ "--style=" <> formattedBaseSty, BS.pack path ] }
          where
            formattedBaseSty = formatStyArg $ StyOpts { basedOnStyle = baseSty, additionalOpts = opts }

    parseResumeObject = convert (show @FillError) . preprocessYaml PartialConfig
    parseResumeOptions knownOpts resumeObj = do
      baseStyle <- EC.liftMaybe ("Unable to find `BasedOnStyle` key in the resume file" :: String)
                $ HM.lookup "BasedOnStyle" resumeObj ^? _Just . _String
      opts <- convert (show @FillError) $ collectConfigItems knownOpts resumeObj
      pure (baseStyle, opts)

liftEither' :: (MonadError String m, Show e) => String -> Either e a -> m a
liftEither' context = liftEither . first ((context <>) . show)

parseOptsDescription :: LBS.ByteString -> Either String (OptsDescription 'Supported)
parseOptsDescription contents = do
  parseResult <- liftEither' "Unable to parse the file: " $ parseDescr contents
  let supportedOptions = filterParsedItems parseResult
  baseStyles <- case find ((== bosKey) . name) supportedOptions of
                     Nothing -> throwError "No `BasedOnStyle` option"
                     Just stys -> pure stys
  let varyingOptions = filter ((/= bosKey) . name) supportedOptions
  case value baseStyles of
       CTEnum { .. } -> pure $ OptsDescription { baseStyles = variants, knownOpts = varyingOptions }
       _ -> throwError [i|Unknown type for the `BaseStyles` option: #{value baseStyles}|]
  where
    bosKey = ["BasedOnStyle"]
