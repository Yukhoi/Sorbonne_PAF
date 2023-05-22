
module TextStatSpec where

import Data.Set as Set 

import Data.Sequence as Seq

import Stats

import Test.Hspec

compteMotSpec text = do
  describe "compteMot" $ do

    it "returns the number of words in the text" $ do
      compteMot text
        `shouldBe` 722807

compteLigneSpec text = do
  describe "compteLigne" $ do

    it "returns the numbers of lines in the text" $ do
      compteLigne text
        `shouldBe` 104094

compteLettreSpec text = do
  describe "compteLettre" $ do

    it "returns the numbers of letter a in the text" $ do
      compteLettre 'a' text
        `shouldBe` 45912
    
    it "returns the numbers of blank space in the text" $ do
      compteLettre ' ' text
        `shouldBe` 104387

    it "returns the numbers of letter e in the text" $ do
    compteLettre 'e' text
    `shouldBe` 45912

engineSpec text = do
    compteMotSpec text
    compteLigneSpec text
    compteLettreSpec text