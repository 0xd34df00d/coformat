{-# LANGUAGE QuasiQuotes, ParallelListComp, RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE DataKinds, GADTs #-}

module Clang.Format.DescrParser
( parseFile
, parseDescr
) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Char
import Data.String.Interpolate
import Control.Monad.Extra
import Text.HTML.DOM
import Text.XML hiding(parseLBS)
import Text.XML.Cursor
import Text.XML.Scraping
import Text.XML.Selector.TH
import Text.XML.Selector.Types

import Clang.Format.Descr

parseFile :: FilePath -> IO (Either String [ConfigItemT 'Parsed])
parseFile = fmap parseDescr . LBS.readFile

parseDescr :: LBS.ByteString -> Either String [ConfigItemT 'Parsed]
parseDescr = parseCursor . fromDocument . parseLBS

parseCursor :: Cursor -> Either String [ConfigItemT 'Parsed]
parseCursor cur = do
  items <- mapM parseItem [ (header, body) | header <- [jq|dl.docutils > dt|] `queryT` cur
                                           | body   <- [jq|dl.docutils > dd|] `queryT` cur
                                           ]
  braceWrappingKludge items

braceWrappingKludge :: [ConfigItemT 'Parsed] -> Either String [ConfigItemT 'Parsed]
braceWrappingKludge = concatMapM f
  where
    f c@ConfigItem { .. } | name /= "BraceWrapping" = pure [c]
    f ConfigItem { typ = CTEnum { .. } } = pure [ ConfigItem { name = "BraceWrapping." <> var, typ = CTBool () }
                                                | var <- variants
                                                , isUpper $ T.head var
                                                ]
    f _ = Left "Expected BraceWrapping to have (mis)type of CTEnum"

parseItem :: (Cursor, Cursor) -> Either String (ConfigItemT 'Parsed)
parseItem (header, body) = do
  name <- header @>. [jq|strong|]
  typStr <- header @>. [jq|span.pre|]
  typ <- parseType name typStr body
  pure ConfigItem { .. }

parseType :: T.Text -> T.Text -> Cursor -> Either String (ConfigTypeT 'Parsed)
parseType name typStr cur
  | Just typ <- lookup typStr variantless = pure typ
  | otherwise = do
      let allOpts = [jq|li > p.first|] `queryT` cur
      let variants
            | null allOpts = dropEnums $ TL.toStrict . innerText <$> [jq|li code.docutils > span.pre|] `queryT` cur
            | otherwise = [ h
                          | opt <- allOpts
                          , let codeBlocks = TL.toStrict . innerText <$> [jq|span.pre|] `queryT` opt
                          , let (h:_) = dropEnums codeBlocks
                          ]
      when (null variants) $ Left [i|no variants found for `#{typStr}` for `#{name}`|]
      pure CTEnum { enumValue = (), .. }
  where
    variantless = [ ("int", CTInt ())
                  , ("bool", CTBool ())
                  , ("unsigned", CTUnsigned ())
                  , ("std::string", CTString ())
                  , ("std::vector<std::string>", CTStringVec ())
                  , ("std::vector<RawStringFormat>", CTRawStringFormats ())
                  , ("std::vector<IncludeCategory>", CTIncludeCats ())
                  ]
    dropEnums = filter (not . T.any (== '_'))

(@>) :: Cursor -> [JQSelector] -> Either String Cursor
cur @> expr | (sub:_) <- queryT expr cur = pure sub
            | NodeElement el <- node cur
            , let el' = el { elementNodes = [] } = Left [i|nothing found for expression #{expr} under element #{el'}|]
            | otherwise = Left [i|nothing found for expression #{expr}|]

(@>.) :: Cursor -> [JQSelector] -> Either String T.Text
cur @>. expr = TL.toStrict . innerText <$> cur @> expr
