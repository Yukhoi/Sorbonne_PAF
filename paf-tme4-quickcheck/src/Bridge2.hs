module Bridge2 where

import Bridge as BR



-- | invariant Bridge 2
prop_inv_IslandBridge2 :: IslandBridge -> Bool
prop_inv_IslandBridge2 b@(BridgeOpened lim nbTo nbI nbFrom) =
  ((nbTo /= 0) && (nbFrom == 0) && 0 <= nbTo + nbI && nbTo + nbI <= lim) || ((nbTo == 0) && (nbFrom /= 0) && 0 <= nbFrom + nbI && nbFrom + nbI <= lim) || (nbTo == 0 && nbFrom == 0)
prop_inv_IslandBridge2 b@(BridgeClosed lim nbTo nbFrom) =
  ((nbTo /= 0) && (nbFrom == 0) && ((nbCars b) == lim)) || ((nbTo == 0) && (nbFrom /= 0) && ((nbCars b) == lim)) || ((nbTo == 0) && (nbFrom == 0) && ((nbCars b) == lim))


-- Preconditions des operations (car les operations restent les memes)

-- | Precondition enterToIsland
prop_pre_enterToIsland2 :: IslandBridge -> Bool
prop_pre_enterToIsland2 b@(BridgeOpened lim _ _ nbFrom) = (nbCars b < lim) && (nbFrom == 0)         -- Il faut qu'il n'y ait pas de voitures dans l'autre sens et que le nombre de 
prop_pre_enterToIsland2 (BridgeClosed _ _ _) = False                                                -- voitures nbTo soit < lim

-- | Precondition leaveToIsland
prop_pre_leaveToIsland2 :: IslandBridge -> Bool  
prop_pre_leaveToIsland2 (BridgeOpened lim nbTo nbI nbFrom) = nbTo > 0 && nbFrom == 0       -- La voiture qui quitte le pont doit exister si le pont n'est pas ferme
prop_pre_leaveToIsland2 (BridgeClosed lim nbTo nbFrom) = nbTo > 0 && nbFrom == 0           -- La voiture qui quitte le pont doit exister si le pont est ferme

-- | Precondition enterFromIsland
prop_pre_enterFromIsland2 :: IslandBridge -> Bool
prop_pre_enterFromIsland2 b@(BridgeOpened lim nbTo nbI nbFrom) = (nbCars b < lim) && (nbI > 0) && (nbTo == 0)   -- Il faut que des voitures existent sur l'ile et qu'il n'y ait pas de voiture
prop_pre_enterFromIsland2 (BridgeClosed _ _ _) = False                                                          -- en sens inverse et que le nombre de nbFrom < lim

-- | Precondition leaveFromIsland
prop_pre_leaveFromIsland2 :: IslandBridge -> Bool
prop_pre_leaveFromIsland2 (BridgeOpened lim nbTo nbI nbFrom) = nbFrom > 0 && nbTo == 0       -- La voiture quittant le pont doit exister que qu'il n'y ait pas de voiture dans l'autre sens
prop_pre_leaveFromIsland2 (BridgeClosed lim nbTo nbFrom) = nbFrom > 0 && nbTo == 0           
