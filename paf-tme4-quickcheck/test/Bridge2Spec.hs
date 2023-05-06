module Bridge2Spec where

import Test.Hspec
import Test.QuickCheck

import Bridge as BR
import Bridge2


property_inv_initBridge2 :: Int -> Property
property_inv_initBridge2 lim =
  (prop_pre_initBridge lim) ==> property $ prop_inv_IslandBridge2 (initBridge lim)

bridgeSpecInit2 = do
  describe "initBridge2" $ do
    it "preserves the invariant" $ property $ \lim -> property_inv_initBridge2 lim


-- Exemple de générateur aléatoire monadique (cf. cours 8)
genBridgeFromFree :: Gen IslandBridge
genBridgeFromFree = do
    lim <- choose (1, 100)  -- la limite initiale
    nbI <- choose (0, lim)
    nbTo <- choose (0, 0)
    nbFrom <- choose (0, lim-nbI-nbTo)
    return $ if nbI + nbTo + nbFrom == lim
             then BridgeClosed lim nbTo nbFrom
             else BridgeOpened lim nbTo nbI nbFrom

-- Exemple de générateur garantissant l'invariant
genBridgeFromOk :: Gen IslandBridge
genBridgeFromOk = do
    lim <- choose (1, 100)  -- la limite initiale
    nbCars <- choose (0, lim)  -- 
    nbI <- choose (0, nbCars)
    nbTo <- choose (0, 0)
    let nbFrom = nbCars - (nbI + nbTo)
    return $ mkBridge lim nbTo nbI nbFrom

genBridgeToFree :: Gen IslandBridge
genBridgeToFree = do
    lim <- choose (1, 100)  -- la limite initiale
    nbI <- choose (0, lim)
    nbFrom <- choose (0, 0)
    nbTo <- choose (0, lim-nbI-nbFrom)
    return $ if nbI + nbTo + nbFrom == lim
             then BridgeClosed lim nbTo nbFrom
             else BridgeOpened lim nbTo nbI nbFrom

genBridgeToOk :: Gen IslandBridge
genBridgeToOk = do
    lim <- choose (1, 100)  -- la limite initiale
    nbCars <- choose (0, lim)  -- 
    nbI <- choose (0, nbCars)
    nbTo <- choose (0, nbCars-nbI)
    let nbFrom = 0
    return $ mkBridge lim nbTo nbI nbFrom


property_inv_genBridgeFromOK2 :: Property
property_inv_genBridgeFromOK2 = forAll genBridgeFromOk $ prop_inv_IslandBridge2

bridgeSpecFromGenOk2 = do
  describe "genBridgeOk2" $ do
    it "generates bridges that satisfy their invariant when nbTo == 0" $
      property property_inv_genBridgeFromOK2

property_inv_genBridgeToOK2 :: Property
property_inv_genBridgeToOK2 = forAll genBridgeToOk $ prop_inv_IslandBridge2

bridgeSpecToGenOk2 = do
  describe "genBridgeOk2" $ do
    it "generates bridges that satisfy their invariant when nbFrom == 0" $
      property property_inv_genBridgeToOK2


property_inv_genBridgeFromFree2 :: Property
property_inv_genBridgeFromFree2 = forAll genBridgeFromFree $ prop_inv_IslandBridge2

bridgeSpecGenFromFree2 = do
  describe "genBridgeFree2" $ do
    it "generates bridges that satisfy their invariant when nbTo == 0" $
      property property_inv_genBridgeFromFree2

property_inv_genBridgeFromTo2 :: Property
property_inv_genBridgeFromTo2 = forAll genBridgeToFree $ prop_inv_IslandBridge2

bridgeSpecGenToFree2 = do
  describe "genBridgeFree2" $ do
    it "generates bridges that satisfy their invariant when nbFrom == 0" $
      property property_inv_genBridgeFromTo2

-- Générateur par défaut, qui peut être incohérent dans max. 20% des cas
-- (ce qui permet de tester les préconditions)
instance Arbitrary IslandBridge where
  arbitrary =
    frequency [(1, genBridgeFromFree)    -- 10% de génération libre
              ,(1, genBridgeToFree)      -- 10% de génération libre
              ,(4, genBridgeToOk)        -- 40% de génération sûre
              ,(4, genBridgeFromOk)]     -- 40% de génération sûre


------------------------------------------------------------ enterToIsland2 ------------------------------------------------------------
property_inv_enterToIsland2 :: IslandBridge -> Property
property_inv_enterToIsland2 b =
  (prop_inv_IslandBridge2 b)
  && (prop_pre_enterToIsland2 b)
  ==> classify (bridgeLimit b < 10) "small capacity (<10)" $
  classify (bridgeLimit b < 50) "medium capacity (<50)" $
  classify (bridgeLimit b <= 100) "large capacity (>=50)" $
  property $ prop_inv_IslandBridge2 (enterToIsland b)

-- >>> quickCheck prop_enterToIsland_inv

enterToIsland2Spec = do
  describe "enterToIsland2" $ do
    it "preserves the invariant" $
      property property_inv_enterToIsland2


------------------------------------------------------------ leaveToIsland2 ------------------------------------------------------------

property_inv_leaveToIsland2 :: IslandBridge -> Property
property_inv_leaveToIsland2 b =
  (prop_inv_IslandBridge2 b)
  && (prop_pre_leaveToIsland2  b)
  ==> classify (bridgeLimit b < 10) "small capacity (<10)" $
  classify (bridgeLimit b < 50) "medium capacity (<50)" $
  classify (bridgeLimit b <= 100) "large capacity (>=50)" $
  property $ prop_inv_IslandBridge2 (leaveToIsland b)

leaveToIsland2Spec = do
  describe "leaveToIsland2" $ do
    it "preserves the invariant" $
      property property_inv_leaveToIsland2

------------------------------------------------------------ enterFromIsland2 ------------------------------------------------------------

property_inv_enterFromIsland2 :: IslandBridge -> Property
property_inv_enterFromIsland2 b =
  (prop_inv_IslandBridge2 b)
  && (prop_pre_enterFromIsland2  b)
  ==> classify (bridgeLimit b < 10) "small capacity (<10)" $
  classify (bridgeLimit b < 50) "medium capacity (<50)" $
  classify (bridgeLimit b <= 100) "large capacity (>=50)" $
  property $ prop_inv_IslandBridge2 (enterFromIsland b)

enterFromIsland2Spec = do
  describe "enterFromIsland" $ do
    it "preserves the invariant" $
      property property_inv_enterFromIsland2


------------------------------------------------------------ leaveFromIsland2 ------------------------------------------------------------

property_inv_leaveFromIsland2 :: IslandBridge -> Property
property_inv_leaveFromIsland2 b =
  (prop_inv_IslandBridge2 b)
  && (prop_pre_leaveFromIsland2  b)
  ==> classify (bridgeLimit b < 10) "small capacity (<10)" $
  classify (bridgeLimit b < 50) "medium capacity (<50)" $
  classify (bridgeLimit b <= 100) "large capacity (>=50)" $
  property $ prop_inv_IslandBridge2 (leaveFromIsland b)

leaveFromIsland2Spec = do
  describe "leaveFromIsland2" $ do
    it "preserves the invariant" $
      property property_inv_leaveFromIsland2

------------------------------------------------------------------------------------------------------------------------------------------