import Test.Hspec

import RevrevSpec as RR
import SplitSpec as SP
import BridgeSpec as BS
import Bridge2Spec as BS2

main :: IO ()
main = hspec $ do
  -- revrev
  RR.revrevSpec
  RR.revappSpec
  -- split
  SP.splitSpec0
  SP.splitSpec1
  SP.splitSpec2
  SP.splitSpec3
  -- -- bridge
  BS.bridgeSpecInit
  BS.bridgeSpecGenOk
  BS.bridgeSpecGenFree
  BS.enterToIslandSpec
  BS.leaveToIslandSpec
  BS.enterFromIslandSpec
  BS.leaveFromIslandSpec
  -- bridge 2
  BS2.bridgeSpecInit2
  BS2.bridgeSpecFromGenOk2
  BS2.bridgeSpecToGenOk2
  BS2.bridgeSpecGenFromFree2
  BS2.bridgeSpecGenToFree2
  BS2.enterToIsland2Spec
  BS2.leaveToIsland2Spec
  BS2.enterFromIsland2Spec
  BS2.leaveFromIsland2Spec
