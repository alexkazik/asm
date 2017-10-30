module Asm.Core.Phase3.Pool where

import           Asm.Core.Prelude
import qualified Data.Map.Strict                as M
import qualified Data.Vector                    as V

import           Asm.Core.Control.CompilerError
import           Asm.Core.Data.ByteVal
import           Asm.Core.Data.ByteValPiece
import           Asm.Core.Data.Cpu
import           Asm.Core.Data.Reference
import           Asm.Core.Phase3.CompilerState3
import           Asm.Core.Phase4.Data.Expr4
import           Asm.Core.Phase4.Data.PoolData
import           Asm.Core.Phase4.Data.Stmt4
import           Asm.Core.SourcePos

-- all following functions throws an fatal error, which is recovered by placeInPoolC

addCodeToPoolC :: Cpu c => Location -> (Reference, Bool) -> Stmt4Block c -> CSM3 c ()
addCodeToPoolC loc (poolN, isVirtual) block = do
  when isVirtual $ $throwFatalError [(loc, "1/selected pool " ++ show poolN ++ " is virtual")]
  modify (\s -> s{cs3PoolData = M.adjust (addBlockToStartPool block) poolN (cs3PoolData s)})

addDataToPoolC :: Cpu c => Location -> (Reference, Bool, Location) -> ByteValPiece (Expr4 c) -> CSM3 c ()
addDataToPoolC loc (poolN, isVirt, poolLoc) block = do
  unless (not isVirt || V.all onlyVirtualData (bvpBytes block)) $
        $throwFatalError
          [ (loc, "3/selected pool " ++ show poolN ++ " is virtual, but the data is not")
          , (poolLoc, "Pool is defined at")
          ]
  modify (\s -> s{cs3PoolData = M.adjust (addVariableToDataPool block) poolN (cs3PoolData s)})
  where
    onlyVirtualData ByteValAny      = True
    onlyVirtualData (ByteValInit w) = w == maxBound
    onlyVirtualData ByteValLocal{}  = True
    onlyVirtualData _               = False

addPoolToPoolC :: Cpu c => Location -> (Reference, Bool) -> Reference -> (Reference, Bool) -> ConstOrInit -> CSM3 c ()
addPoolToPoolC loc (s, sVirt) d (poolN, poolVirt) constOrInit = do
  unless (not poolVirt || sVirt) $  -- destination is not a virtual pool, or source + destination are
      $throwFatalError
        [ (loc, "6/selected pool " ++ show poolN ++ " is virtual, but the data is not")
        , ([], "Pool is defined at")
        ]
  modify (\st -> st{cs3PoolData = M.adjust (addPoolToPoolPoolC (s, d, constOrInit)) poolN (cs3PoolData st)})
