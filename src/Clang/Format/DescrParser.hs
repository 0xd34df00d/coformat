{-# LANGUAGE QuasiQuotes, ParallelListComp, RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE DataKinds, GADTs #-}

module Clang.Format.DescrParser
( parseDescr
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

import Language.Coformat.Descr

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
    f c@ConfigItem { .. } | name /= ["BraceWrapping"] = pure [c]
    f ConfigItem { value = CTEnum { .. } } = pure [ ConfigItem { name = ["BraceWrapping", var], value = CTBool () }
                                                  | var <- variants
                                                  , isUpper $ T.head var
                                                  ]
    f _ = Left "Expected BraceWrapping to have (mis)type of CTEnum"

parseItem :: (Cursor, Cursor) -> Either String (ConfigItemT 'Parsed)
parseItem (header, body) = do
  nameToken <- header @>. [jq|strong|]
  typStr <- header @>. [jq|span.pre|]
  value <- parseType nameToken typStr body
  let name = [nameToken]
  pure ConfigItem { .. }

parseType :: T.Text -> T.Text -> Cursor -> Either String (ConfigTypeT 'Parsed)
parseType name typStr cur
  | Just typ <- lookup typStr variantless = pure typ
  | otherwise = do
      let allVars = TL.toStrict . innerText <$> [jq|li code.docutils > span.pre|] `queryT` cur
      let unEnumed = [ T.tail rest
                     | var <- allVars
                     , let (_, rest) = T.break (== '_') var
                     , not $ T.null rest
                     ]
      let variants = if null unEnumed then allVars else unEnumed
      when (null variants) $ Left [i|no variants found for `#{typStr}` for `#{name}`|]
      pure CTEnum { enumValue = (), .. }
  where
    variantless = [ ("int", CTInt ())
                  , ("bool", CTBool ())
                  , ("unsigned", CTUnsigned ())
                  , ("std::string", CTUnsupported ())
                  , ("std::vector<std::string>", CTUnsupported ())
                  , ("std::vector<RawStringFormat>", CTUnsupported ())
                  , ("std::vector<IncludeCategory>", CTUnsupported ())
                  ]

(@>) :: Cursor -> [JQSelector] -> Either String Cursor
cur @> expr | (sub:_) <- queryT expr cur = pure sub
            | NodeElement el <- node cur
            , let el' = el { elementNodes = [] } = Left [i|nothing found for expression #{expr} under element #{el'}|]
            | otherwise = Left [i|nothing found for expression #{expr}|]

(@>.) :: Cursor -> [JQSelector] -> Either String T.Text
cur @>. expr = TL.toStrict . innerText <$> cur @> expr
