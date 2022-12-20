module Day13Spec (spec) where

import Test.Hspec
import Day13

ex1 = [ "[1,1,3,1,1]", "[1,1,5,1,1]", "",
        "[[1],[2,3,4]]", "[[1],4]", "",
        "[9]", "[[8,7,6]]", "",
        "[[4,4],4,4]", "[[4,4],4,4,4]", "",
        "[7,7,7,7]", "[7,7,7]", "",
        "[]", "[3]", "",
        "[[[]]]", "[[]]", "",
        "[1,[2,[3,[4,[5,6,7]]]],8,9]", "[1,[2,[3,[4,[5,6,0]]]],8,9]",""]

spec :: Spec 
spec = do
    describe "toTokens" $ do
        it "tokenizes a line" $ do
            (toTokens  "[[1],[2,3,4]]") `shouldBe` ["[","[","1","]",",","[","2",",","3",",","4","]","]"]

    describe "parse" $ do
        it "parse the tokens to a simple expression" $ do
            (parse ["1","0"]) `shouldBe` ([],List [One 1, One 0],False)
        it "parse the tokens to an expression with a list" $ do
            (parse ["[","1","0","]"]) `shouldBe` ([],List [List [One 1, One 0]],False)
        it "parse the tokens to an expression with a list in a list" $ do
            (parse ["[","1","0","]","6"]) `shouldBe` ([],List [List [One 1, One 0], One 6],False)

    describe "compareString" $ do
        it "compares pair 1" $ do
            (compareString "[1,1,3,1,1]" "[1,1,5,1,1]") `shouldBe` (Just True)
        it "compares pair 2" $ do
            (compareString "[[1],[2,3,4]]" "[[1],4]") `shouldBe` (Just True)
        it "compares pair 3" $ do
            (compareString "[9]" "[[8,7,6]]") `shouldBe` (Just False)
        it "compares pair 4" $ do
            (compareString "[[4,4],4,4]" "[[4,4],4,4,4]") `shouldBe` (Just True)
        it "compares pair 5" $ do
            (compareString "[7,7,7,7]" "[7,7,7]") `shouldBe` (Just False)
        it "compares pair 6" $ do
            (compareString "[]" "[3]") `shouldBe` (Just True)
        it "compares pair 7" $ do
            (compareString "[[[]]]" "[[]]") `shouldBe` (Just False)
        it "compares pair 8" $ do
            (compareString "[1,[2,[3,[4,[5,6,7]]]],8,9]" "[1,[2,[3,[4,[5,6,0]]]],8,9]") `shouldBe` (Just False)

    describe "sumIndexGoodPairs" $ do
        it "sumIndexGoodPairs" $ do
            (sumIndexGoodPairs ex1) `shouldBe` 13

    describe "decoderKey" $ do
        it "decodes the key for some packets" $ do
            (decoderKey ex1) `shouldBe` 140