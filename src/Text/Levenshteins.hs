module Text.Levenshteins
( blinding
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

blinding :: String -> String -> Int
blinding s1 s2 = levenshteinDistance defaultEditCosts (blindTokens s1) (blindTokens s2)
