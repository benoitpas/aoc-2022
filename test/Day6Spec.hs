module Day6Spec (spec) where

import Test.Hspec
import Day6

import qualified Data.Map as M

ex1 :: String
ex1 = "bvwbjplbgvbhsrlpgdmjqwftvncz"
ex2 = "nppdvjthqldpwncqszvftbrmjlhg" -- : first marker after character 6
ex3 = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" -- : first marker after character 10
ex4 = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"

spec :: Spec
spec = do
  describe "findMarker" $ do
    it "finds the marker for example 1" $ do
        (findMarker ex1) `shouldBe` 5
  describe "findMarker" $ do
    it "finds the marker for example 2" $ do
        (findMarker ex2) `shouldBe` 6
  describe "findMarker" $ do
    it "finds the marker for example 3" $ do
        (findMarker ex3) `shouldBe` 10
  describe "findMarker" $ do
    it "finds the marker for example 4" $ do
        (findMarker ex4) `shouldBe` 11