module Asm.Core.Phase3.Pool where

import           Asm.Core.Prelude
import qualified Data.Map.Strict                as M
import qualified Data.Vector                    as V

import           Asm.Core.Data.ByteVal
import           Asm.Core.Data.ByteValPiece
import           Asm.Core.Data.Cpu
import           Asm.Core.Data.Reference
import           Asm.Core.Phase3.CompilerState3
import           Asm.Core.Phase4.Data.Expr4
import           Asm.Core.Phase4.Data.PoolData
import           Asm.Core.Phase4.Data.Stmt4
import           Asm.Core.SourcePos


addCodeToPoolC :: Cpu c => Location -> (Reference, Bool) -> Stmt4Block c -> CSM3 c ()
addCodeToPoolC loc (poolN, True) _ =
  printErrorC $
    (loc, "1/selected pool " ++ show poolN ++ " is virtual"):[sourcePos||]
addCodeToPoolC _ (poolN, False) block = state addCodeToPoolC'
  where
    addCodeToPoolC' s@CSt3 {cs3PoolData} = ((), s{cs3PoolData = M.adjust (addBlockToStartPool block) poolN cs3PoolData})

addDataToPoolC :: Cpu c => Location -> (Reference, Bool, Location) -> ByteValPiece (Expr4 c) -> CSM3 c ()
addDataToPoolC loc (poolN, isVirt, poolLoc) block =
  if not isVirt || V.all onlyVirtualData (bvpBytes block)
    then state addDataToPoolC'
    else
        printErrorC $
          (loc, "3/selected pool " ++ show poolN ++ " is virtual, but the data is not") :
          (poolLoc, "Pool is defined at") :
          [sourcePos||]
  where
    addDataToPoolC' s@CSt3 {cs3PoolData} = ((), s{cs3PoolData = M.adjust (addVariableToDataPool block) poolN cs3PoolData})
    onlyVirtualData ByteValAny      = True
    onlyVirtualData (ByteValInit w) = w == maxBound
    onlyVirtualData ByteValLocal{}  = True
    onlyVirtualData _               = False

addPoolToPoolC :: Cpu c => Location -> (Reference, Bool) -> Reference -> (Reference, Bool) -> ConstOrInit -> CSM3 c ()
addPoolToPoolC loc (s, sVirt) d (poolN, poolVirt) constOrInit =
  if not poolVirt || sVirt -- destination is not a virtual pool, or source + destination are
    then state addPoolToPoolC'
    else
      printErrorC $
        (loc, "6/selected pool " ++ show poolN ++ " is virtual, but the data is not") :
        ([], "Pool is defined at") :
        [sourcePos||]
  where
    addPoolToPoolC' st@CSt3 {cs3PoolData} = ((), st{cs3PoolData = M.adjust (addPoolToPoolPoolC (s, d, constOrInit)) poolN cs3PoolData})
