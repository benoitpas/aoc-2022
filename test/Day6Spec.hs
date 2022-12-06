module Day6Spec (spec) where

import Test.Hspec
import Day6

ex1 :: String
ex1 = "bvwbjplbgvbhsrlpgdmjqwftvncz"

ex2 :: String
ex2 = "nppdvjthqldpwncqszvftbrmjlhg"

ex3 :: String
ex3 = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"

ex4 :: String
ex4 = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"

spec :: Spec
spec = do
  describe "findMarker" $ do
    it "finds the marker for example 1" $ do
        (findMarker 4 ex1) `shouldBe` 5
  describe "findMarker" $ do
    it "finds the marker for example 2" $ do
        (findMarker 4 ex2) `shouldBe` 6
  describe "findMarker" $ do
    it "finds the marker for example 3" $ do
        (findMarker 4 ex3) `shouldBe` 10
  describe "findMarker" $ do
    it "finds the marker for example 4" $ do
        (findMarker 4 ex4) `shouldBe` 11
  describe "findMarker" $ do
    it "finds the message for example 1" $ do
        (findMarker 14 ex1) `shouldBe` 23
  describe "findMarker" $ do
    it "finds the message for example 2" $ do
        (findMarker 14 ex2) `shouldBe` 23
  describe "findMarker" $ do
    it "finds the message for example 3" $ do
        (findMarker 14 ex3) `shouldBe` 29
  describe "findMarker" $ do
    it "finds the message for example 4" $ do
        (findMarker 14 ex4) `shouldBe` 26