{-# LANGUAGE TupleSections #-}

module Language.Coformat.Language where

import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty
import Data.Maybe
import System.FilePath.Posix

data Language = Cpp
  deriving (Eq, Ord, Show)

guessLanguage :: NonEmpty FilePath -> Either String Language
guessLanguage files = case langs of
                           [] -> Left "Could not detect any languages"
                           [lang] -> Right lang
                           _ -> Left "More than one language detected"
  where
    langs = mapMaybe extToFile $ toList $ takeExtension <$> files

extToFile :: String -> Maybe Language
extToFile ext = HM.lookup ext exts
  where
    exts = HM.fromList $ (, Cpp) <$> cpps
    cpps = ["cpp", "c", "h", "cxx", "hpp", "hxx"]
