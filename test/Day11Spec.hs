module Day11Spec (spec) where

import Test.Hspec
import Day11

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
        "    If false: throw to monkey 1",
        "     [\"noop\",",
        "        \"addx 3\",",
        "        \"addx -5\","]

spec :: Spec
spec = do
    describe "parseMonkey" $ do
        it "parse the information abour a monkey" $ do
            (let Just(_,m,_0) = parseMonkey ex1 in m) `shouldBe` Monkey [79,98] (*2) (\_->True) 2 3
