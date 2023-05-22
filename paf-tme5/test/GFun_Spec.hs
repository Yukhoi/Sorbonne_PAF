module GFun_Spec where

import GFun

import Test.Hspec
import Control.Exception (evaluate)

-- some example series
negativeOnes = Zero (-1) negativeOnes
ones = Zero 1 GFun_Spec.ones
twos = Zero 2 twos
threes = Zero 3 threes
fours = Zero 4 fours
fives = Zero 5 fives

-- check takeSeries function

takeSeriesSpec = do
    describe "takeSeriesSpec" $ do

        it "should return a list of elements from a series" $ do
            takeSeries 10 twos `shouldBe` (take 10 (repeat 2))

        it "should return an empty list when the input number is 0" $ do
            takeSeries 0 twos `shouldBe` []

fmapSpec = do
    describe "fmapSpec" $ do

        it "should apply a function to each element in a series" $ do
            fmap (*2) twos `shouldBe` fours

instanceNumSpec = do
    describe "instanceNumSpec" $ do

        it "should support addition of two series" $ do
            twos + threes `shouldBe` fives

        it "should support subtraction of two series" $ do
            threes - twos `shouldBe` GFun_Spec.ones

        it "should support multiplication of two series" $ do
            takeSeries 10 (twos * twos) `shouldBe` (take 10 (iterate (+4) 4))

        it "should support computation of the absolute values of a series" $ do
            abs (twos - threes) `shouldBe` GFun_Spec.ones

        it "should support computation of the signum of values in a series" $ do
            signum (negate twos) `shouldBe` negativeOnes

        it "should support generation of a series from an integer" $ do
            (fromInteger 4 :: Series Int) `shouldBe` fours

engineSpec = do
    takeSeriesSpec
    fmapSpec
    instanceNumSpec
