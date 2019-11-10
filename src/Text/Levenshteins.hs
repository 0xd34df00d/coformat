module Text.Levenshteins
( blindTokens
, dropStartSpaces

, levenshteinDistanceWith
) where

import qualified Data.Sequence as S
import Data.Char
import Data.Foldable
import Data.Sequence(Seq(..))
import Text.EditDistance

blindTokens :: String -> String
blindTokens = toList . finalize . foldl' f (False, mempty) . S.fromList
  where
    finalize (False, out) = out
    finalize (True, out) = out :|> '$'
    f (hadToken, out) ch
      | isAlpha ch = (True, out)
      | hadToken = (False, out :|> '$' :|> ch)
      | otherwise = (False, out :|> ch)

dropStartSpaces :: String -> String
dropStartSpaces = unlines . map (dropWhile isSpace) . lines

levenshteinDistanceWith :: (String -> String) -> String -> String -> Int
levenshteinDistanceWith f s1 s2 = levenshteinDistance defaultEditCosts  (f s1) (f s2)
