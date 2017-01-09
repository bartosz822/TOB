import Test.Hspec
import Test.HUnit
import Test.QuickCheck
import Stats
{-# ANN module ("HLint: ignore Redundant $"::String) #-}
{-# ANN module ("HLint: ignore Redundant do"::String) #-}

main :: IO ()
main = hspec $ do
    describe "Tests" $ do
        it ("Should work for" ++ s1) $ do
          stats s1 `shouldBe` r1

        it "Should show Chars" $
          property $ statProp

        it ("Should work for" ++ s2) $
          test1

        it "Shoould properly display chars for any string" $
          property $ statProp2


s1 = "jeden dwa trzy"
r1 = "Chars :14 Words :3 Different Chars :11 Different Words :3 "

s2 = "abcdefghijklmnopqrstuwxyz aaaa bbbb cccc dddd"
r2 = "Chars :45 Words :5 Different Chars :26 Different Words :5 "

test1 :: Assertion
test1 = stats s2 @?= r2


statProp :: String -> Bool
statProp s = take 5 (stats s) == "Chars"

statProp2 s = take (7 +  (length.show.length $ s)) (stats  s) == "Chars :" ++ (show.length $ s)