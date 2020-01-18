{-# LANGUAGE GADTs, TypeApplications, DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Clang.Format.Formatter(clangFormatter) where

import qualified Control.Monad.Except.CoHas as EC
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import Control.Lens
import Data.Aeson.Lens

import Clang.Format.DescrParser
import Clang.Format.StyOpts
import Clang.Format.YamlConversions
import Language.Coformat.Descr
import Language.Coformat.Formatter
import Language.Coformat.Util

clangFormatter :: Formatter
clangFormatter = Formatter { .. }
  where
    formatterInfo = FormatterInfo { .. }
      where
        execName = "clang-format"
        formatterOpts = StaticOpts $(staticOptions "data/ClangFormatStyleOptions-9.html")
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

        serializeOptions baseSty opts = formatClangFormat StyOpts { basedOnStyle = baseSty, additionalOpts = opts }

    parseResumeObject = convert (show @FillError) . preprocessYaml PartialConfig
    parseResumeOptions knownOpts resumeObj = do
      baseStyle <- EC.liftMaybe ("Unable to find `BasedOnStyle` key in the resume file" :: String)
                $ HM.lookup "BasedOnStyle" resumeObj ^? _Just . _String
      opts <- convert (show @FillError) $ collectConfigItems knownOpts resumeObj
      pure (baseStyle, opts)
