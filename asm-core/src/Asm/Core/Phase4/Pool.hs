module Asm.Core.Phase4.Pool where

import           Asm.Core.Prelude
import qualified Data.Map.Strict                     as M
import qualified Data.Vector                         as V

import           Asm.Core.Data.ByteVal
import           Asm.Core.Data.Cpu
import           Asm.Core.Data.Reference
import           Asm.Core.Phase4.CompilerState4
import           Asm.Core.Phase4.Data.PoolData
import           Asm.Core.Phases.Data.PoolDefinition
import           Asm.Core.Phases34.Data.PoolState
import           Asm.Core.SourcePos
import           Asm.Data.ByteValSimple

getPoolsC :: Cpu c => CSM4 c (Map Reference (PoolData c))
getPoolsC = gets cs4PoolData

getPoolDataC :: Cpu c => Reference -> CSM4 c (PoolData c)
getPoolDataC p = state (\s -> (fromMaybe (printErrorS s $ ([], "getPoolDataC " ++ show p ++ show (M.keys $ cs4PoolData s)):[sourcePos||]) $ M.lookup p (cs4PoolData s), s))

setPoolsC :: Cpu c => Map Reference (PoolData c) -> CSM4 c ()
setPoolsC pools = modify (\s -> s{cs4PoolData = pools})

getPoolDefinitionsC :: Cpu c => CSM4 c (Map Reference PoolDefinition)
getPoolDefinitionsC = asks cs4PoolDefinition

setPoolStateC :: Cpu c => Reference -> PoolState -> CSM4 c ()
setPoolStateC pool ps = modify (\s -> s{cs4PoolState = M.insert pool ps (cs4PoolState s)})

areAllPoolsFinalC :: Cpu c => CSM4 c Bool
areAllPoolsFinalC = gets (\CSt4{..} -> M.foldr areAllPoolsFinalC' True cs4PoolData)
  where
    areAllPoolsFinalC' :: PoolData t1 -> Bool -> Bool
    areAllPoolsFinalC' PoolDataFinal{} b = b
    areAllPoolsFinalC' _ _               = False

getFinalPoolsS :: Cpu c => CompilerState4 c -> (Reference, PoolDefinition) -> (Reference, Int64, Int64, Maybe (Vector ByteValSimple))
getFinalPoolsS s (n,pd) = (n,pdStart pd, fromIntegral $ V.length content, if pdVirtual pd then Nothing else Just content)
  where
    content = map toByteValSimple $ foldr (\n' -> (V.++) $ getByteValVector (cs4PoolData s M.! n')) V.empty $ pdPools pd
