module Stats
  (
    stats

  ) where

import qualified Data.Set as S
import Data.Monoid

stats :: String -> String
stats  = countChars <>  countWords <> differentChars <> differentWords

showLength = show.length

countChars :: String -> String
countChars s = "Chars :" ++ showLength s ++ " "

countWords :: String -> String
countWords s = "Words :" ++ (showLength.words $ s) ++ " "

differentChars :: String -> String
differentChars s = "Different Chars :" ++ (showLength. S.toList . S.fromList $ s )  ++ " "

differentWords :: String -> String
differentWords s = "Different Words :" ++ (showLength. S.toList . S.fromList . words $ s )  ++ " "





