module SplitSpec where

import Test.Hspec
import Test.QuickCheck

import Split

splitSpec0 = do               -- Nom du test HSpec
  describe "split" $ do       -- Precision de quelle fonction on test
    it "splits a string wrt. a given character into a list of words" $        -- Description du test unitaire
      (split '/' "aa/bb/ccc/dd d") `shouldBe` ["aa", "bb", "ccc", "dd d"]     -- Comparaison du resultat du test et du resultat attendu  

splitSpec1 = do               -- Nom du test HSpec
  describe "split" $ do       -- Precision de quelle fonction on test
    it "can be undone with unsplit (v1)" $ property $          -- Description du test avec ajout un test de propriete QuickCheck
      \c xs -> collect (length xs) $ prop_split_unsplit c xs   -- Affichage des tailles des chaines de caracteres pour obtenir la distribution aleatoire 
                                                               -- On verifie la propriete pour 


splitSpec2 = do               -- Nom du test HSpec
  describe "split" $ do       -- Precision de quelle fonction on test
    it "can be undone with unsplit (v2)" $ property $                                       -- Description du test avec ajout un test de propriete QuickCheck
      \xs -> forAll (elements xs) $ \c -> collect (length xs) $ prop_split_unsplit c xs     -- On cree un generateur de type 'String', qu'on donne au forAll pour qu'il
                                                                                            -- teste pour un certain nombre de chaines de caracteres tirees aleatoirement
                                                                                            -- Affichage des tailles des chaines de caracteres pour obtenir la distribution aleatoire  
                                                                                            -- On verifie la propriete pour des chaines de caracteres tirees donc aleatoirement

-- Remarque : on utilise comme caractère de split les éléments des listes `xs` en entrée,
--            cf. la doc QuickCheck sur `forAll`, `elements`, etc.

-- | forAll :: Gen a -> (a -> Property) -> Property
-- Permet de generer des donnees aleatoires pour tester des proprietes, elle teste la propriete pour un certain nombre de valeurs generees aleatoirement.

-- | elements :: [a] -> Gen a
-- Permet de generer une valeur aleatoire a partir d'une liste de valeurs donnees


splitSpec3 = do               -- Nom du test HSpec
  describe "split" $ do       -- Precision de quelle fonction on test
    it "can be undone with unsplit (v3)" $ property $           -- Description du test avec ajout un test de propriete QuickCheck
      forAll (oneof [return "bla bla bli"                       -- Choix d'un generetaur constant cree par return ... et donne a 'forAll' pour qu'il
                     , return "toto"                            -- teste pour un certain nombre de chaines de caracteres de cette liste
                     , return ""
                     , return "un    deux trois   quatre"]) $
      \xs -> prop_split_unsplit ' ' xs                          -- On verifie la propriete pour des chaines de caracteres tirees donc aleatoirement dans cette liste

-- | oneof :: [Gen a] -> Gen a
-- Permet de generer des valeurs aleatoires a partir d'une liste de generateurs 
-- en choisisant aleatoirement l'un des generateurs et utilise ce generateur pour generer une valeur aleatoire
