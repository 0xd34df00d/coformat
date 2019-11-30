module Text.Levenshteins
( blindTokens
, dropStartSpaces
, leaveStartSpaces
, StringNormalizer

, levenshteinDistanceWith
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Sequence as S
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Monad.ST
import Data.Char
import Data.Foldable
import Data.Sequence(Seq(..))
import Text.EditDistance

type StringNormalizer = String -> String

blindTokens :: StringNormalizer
blindTokens = toList . finalize . foldl' f (False, mempty) . S.fromList
  where
    finalize (False, out) = out
    finalize (True, out) = out :|> '$'
    f (hadToken, out) ch
      | isAlpha ch = (True, out)
      | hadToken = (False, out :|> '$' :|> ch)
      | otherwise = (False, out :|> ch)

dropStartSpaces :: StringNormalizer
dropStartSpaces = unlines . map (dropWhile isSpace) . lines

levenshteinDistanceWith :: StringNormalizer -> String -> String -> Int
levenshteinDistanceWith f s1 s2 = levenshteinDistance defaultEditCosts (f s1) (f s2)

leaveStartSpaces :: StringNormalizer
leaveStartSpaces = unlines . map (takeWhile isSpace) . lines

levenshteinDistance' :: BS.ByteString -> BS.ByteString -> Int
levenshteinDistance' s1 s2 = runST $ do
  v0Init <- VUM.unsafeNew $ n + 1
  forM_ [0..n] $ \i -> VUM.unsafeWrite v0Init i i
  v1Init <- VUM.unsafeNew $ n + 1
  v0 <- loop 0 v0Init v1Init
  VUM.unsafeRead v0 n
  where
    m = BS.length s1
    n = BS.length s2

    loop i v0 v1
      | i == m = pure v0
      | otherwise = do
          VUM.unsafeWrite v1 0 (i + 1)
          forM_ [0..n - 1] $ \j -> do
            delCost <- inc $ v0 `VUM.unsafeRead` (j + 1)
            insCost <- inc $ v1 `VUM.unsafeRead` j
            substCost <- if s1 `BS.index` i == s2 `BS.index` j
                         then v0 `VUM.unsafeRead` j
                         else inc $ v0 `VUM.unsafeRead` j
            VUM.unsafeWrite v1 (j + 1) $ minimum [delCost, insCost, substCost]
          loop (i + 1) v1 v0
      where inc = fmap (1 +)
