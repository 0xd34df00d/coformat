{-# LANGUAGE FlexibleContexts, TypeApplications #-}
{-# LANGUAGE DataKinds, RankNTypes, GADTs #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, QuasiQuotes #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Search as BSS
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Control.Concurrent.Async
import Control.Monad.Except
import Control.Monad.Logger
import Data.Bifunctor
import Data.Char
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Ord
import Data.Sequence(Seq(..))
import Data.String.Interpolate.IsString
import Data.Void
import Data.Yaml
import System.Command.QQ
import Text.EditDistance

import Clang.Coformat.Util
import Clang.Format.Descr
import Clang.Format.DescrParser
import Clang.Format.YamlParser

liftEither' :: (MonadError String m, Show e) => String -> Either e a -> m a
liftEither' context = liftEither . first ((context <>) . show)

convert :: (MonadError String m, MonadIO m, Show e)
        => (e -> String)
        -> (forall m'. (MonadError e m', MonadIO m') => m' a)
        -> m a
convert cvt act = runExceptT act >>= (liftEither . first cvt)

parseOptsDescription :: (MonadError String m, MonadIO m) => FilePath -> m ([T.Text], [ConfigItemT 'Supported])
parseOptsDescription path = do
  parseResult <- liftIO $ parseFile path
  supportedOptions <- filterParsedItems <$> liftEither' "Unable to parse the file: " parseResult
  baseStyles <- case find ((== bosKey) . name) supportedOptions of
                     Nothing -> throwError "No `BasedOnStyle` option"
                     Just stys -> pure stys
  let varyingOptions = filter ((/= bosKey) . name) supportedOptions
  case typ baseStyles of
       CTEnum { .. } -> pure (variants, varyingOptions)
       _ -> throwError [i|Unknown type for the `BaseStyles` option: #{typ baseStyles}|]
  where
    bosKey = "BasedOnStyle"

data UserForcedOpts = UserForcedOpts
  { tabWidth :: Maybe Int
  , useTab :: Maybe Bool
  } deriving (Eq, Show)

toConfigItems :: UserForcedOpts -> [ConfigItemT 'Value]
toConfigItems UserForcedOpts { .. } =
  catMaybes [ unpackInt "TabWidth" tabWidth
            , unpackEnum "UseTab" $ (\b -> if b then "Always" else "Never") <$> useTab
            ]
  where
    unpackInt _ Nothing = Nothing
    unpackInt label (Just val) = Just ConfigItem { name = label, typ = CTInt val }
    unpackEnum _ Nothing = Nothing
    unpackEnum label (Just val) = Just ConfigItem { name = label, typ = CTEnum [val] val }

data StyOpts = StyOpts
  { basedOnStyle :: T.Text
  , overriddenOpts :: [ConfigItemT 'Value]
  }

formatStyArg :: StyOpts -> T.Text
formatStyArg StyOpts { .. } = "{ " <> T.intercalate ", " opts <> " }"
  where
    opts = [i|BasedOnStyle: #{basedOnStyle}|]
         : [ [i|#{name}: #{fmtTyp typ}|]
           | ConfigItem { .. } <- overriddenOpts
           ]
    fmtTyp (CTInt n) = show n
    fmtTyp (CTUnsigned n) = show n
    fmtTyp (CTString v) = absurd v
    fmtTyp (CTStringVec v) = absurd v
    fmtTyp (CTRawStringFormats v) = absurd v
    fmtTyp (CTIncludeCats v) = absurd v
    fmtTyp (CTBool b) = if b then "true" else "false"
    fmtTyp (CTEnum _ opt) = T.unpack opt

data Options = Options
  { userFocedOpts :: UserForcedOpts
  , inputFiles :: [String]
  } deriving (Eq, Show)

internalize :: Seq Char -> (Seq Char, HM.HashMap BS.ByteString Char)
internalize = merge . foldl' f (mempty, (mempty, mempty))
  where
    merge (acc, (output, st)) = (output <> acc, st)
    f (acc, (output, st)) ch
      | isAlpha ch = (acc :|> ch, (output, st))
      | S.length acc < 2 = (mempty, (output <> acc :|> ch, st))
      | Just interned <- HM.lookup tokenBS st = (mempty, (output :|> interned :|> ch, st))
      | otherwise = let interned = chr $ 127 + HM.size st
                        st' = HM.insert tokenBS interned st
                    in (mempty, (output :|> interned :|> ch, st'))
      where
        tokenBS = BS.pack $ toList acc

internalize' :: HM.HashMap BS.ByteString Char -> BS.ByteString -> BS.ByteString
internalize' table str = foldl' f str tableList
  where
    tableList = sortBy (flip $ comparing $ BS.length . fst) $ HM.toList table
    f acc (token, rep) = BSL.toStrict $ BSS.replace token (BS.singleton rep) acc

fastLD :: String -> BS.ByteString -> Int
fastLD s1 s2 = levenshteinDistance defaultEditCosts s1' $ BS.unpack s2'
  where
    (dumb, internTable) = first toList $ internalize $ S.fromList s1
    s1' = BS.unpack $ internalize' internTable $ BS.pack s1
    s2' = internalize' internTable s2

chooseBaseStyle :: (MonadError String m, MonadLoggerIO m) => UserForcedOpts -> [T.Text] -> [String] -> m T.Text
chooseBaseStyle ufos baseStyles files = do
  logger <- askLoggerIO
  estimates <- liftIO $ forConcurrently ((,) <$> baseStyles <*> files) $ \(sty, file) -> flip runLoggingT logger $ runExceptT $ do
    let formattedSty = formatStyArg StyOpts { basedOnStyle = sty, overriddenOpts = toConfigItems ufos }
    stdout <- checked [sh|clang-format --style="#{formattedSty}" #{file}|]
    source <- liftIO $ readFile file
    let dist = levenshteinDistance defaultEditCosts source $ TL.unpack stdout
    logDebugN [i|Initial guess for #{sty} at #{file}: #{dist}|]
    pure (sty, dist)
  sty2dists <- liftEither $ sequence estimates
  let accumulated = HM.toList $ HM.fromListWith (+) sty2dists
  forM_ accumulated $ \(sty, acc) -> logInfoN [i|Initial accumulated guess for #{sty}: #{acc}|]
  pure $ fst $ minimumBy (comparing snd) accumulated

doWork :: (MonadError String m, MonadLoggerIO m) => m ()
doWork = do
  (baseStyles, varyingOptions) <- parseOptsDescription "data/ClangFormatStyleOptions-9.html"
  baseStyle <- chooseBaseStyle (UserForcedOpts (Just 4) (Just True)) baseStyles ["data/core.cpp", "data/core.h"]
  stdout <- checked [sh|clang-format --style=#{baseStyle} --dump-config|]
  filledOptions <- convert (show @FillError) $ fillConfigItems varyingOptions $ BSL.toStrict $ TL.encodeUtf8 stdout
  liftIO $ mapM_ print filledOptions

testYaml :: IO ()
testYaml = do
  Right (_, Object res) <- decodeFileWithWarnings @Value "sample.yaml"
  mapM_ print $ sortOn fst $ HM.toList res

main :: IO ()
main = do
  res <- runStderrLoggingT $ runExceptT doWork
  case res of
       Left err -> putStrLn err
       Right () -> putStrLn "done"
