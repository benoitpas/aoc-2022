module Day11Spec (spec) where

import Test.Hspec
import Day11

import qualified Data.Map as M

ex1 = [ "Monkey 0:",
        "  Starting items: 79, 98",
        "  Operation: new = old * 19",
        "  Test: divisible by 23",
        "    If true: throw to monkey 2",
        "    If false: throw to monkey 3",
        "",
        "Monkey 1:",
        "  Starting items: 54, 65, 75, 74",
        "  Operation: new = old + 6",
        "  Test: divisible by 19",
        "    If true: throw to monkey 2",
        "    If false: throw to monkey 0",
        "",
        "Monkey 2:",
        "  Starting items: 79, 60, 97",
        "  Operation: new = old * old",
        "  Test: divisible by 13",
        "    If true: throw to monkey 1",
        "    If false: throw to monkey 3",
        "",
        "Monkey 3:",
        "  Starting items: 74",
        "  Operation: new = old + 3",
        "  Test: divisible by 17",
        "    If true: throw to monkey 0",
        "    If false: throw to monkey 1"]

op = (*1)

tst =  (\_->True)

spec :: Spec
spec = do
    describe "parseMonkey" $ do
        it "parse the information abour a monkey" $ do
            (let Right(_, mk0, l0) = parseMonkey ex1 in mk0) `shouldBe` Monkey [79,98] op tst 2 3

    describe "nextRound" $ do
        it "computes the next round" $ do
            (nextRound (parseMonkeys ex1)) `shouldBe` (M.fromList  [(0, (2,Monkey [20,23,27,26] op tst 2 3)), 
                                                                    (1, (4,Monkey [2080,25,167,207,401,1046] op tst 2 0)),
                                                                    (2, (3,Monkey [] op tst 1 3)), 
                                                                    (3, (5,Monkey [] op tst 0 1))])

    describe "monkeyBusiness" $ do
        it "computes the monkey business level" $ do
            (monkeyBusiness 20 ex1) `shouldBe` 10605