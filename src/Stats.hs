-- Code formatted using stylish Haskell

-- |
-- = Stats module used for getting strings statistics

module Stats
  (
    stats
  ) where

import           Data.Monoid
import qualified Data.Set    as S


-- | Returns statistics for a given string such as :
--     Words
--     Different Chars
--     Different Words
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





