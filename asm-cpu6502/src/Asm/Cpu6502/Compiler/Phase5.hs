module Asm.Cpu6502.Compiler.Phase5
  ( cpu6502CodeAlign
  , cpu6502Stmt5MinMaxLength
  , cpu6502OptimiseLocalStmtBlockC
  , cpu6502OptimiseGlobalStmtBlockC
  , cpu6502StmtBlockToByteValPiece
  ) where

import           Asm.Core.Prelude
import qualified Data.Map.Strict                as M
import qualified Data.Vector                    as V

import           Asm.Core.Data.ByteVal
import           Asm.Core.Data.ByteValPiece
import           Asm.Core.Data.CpuData
import           Asm.Core.Data.Reference
import           Asm.Core.Phase4.CompilerState4
import           Asm.Core.Phase4.Data.Expr4

import           Asm.Cpu6502.Data.CpuData6502
import           Asm.Cpu6502.Data.OpCodes
import           Asm.Cpu6502.OpCodes.Quote


--
-- general
--

cpu6502CodeAlign :: Int64
cpu6502CodeAlign = 1

cpu6502Stmt5MinMaxLength :: CS5 Cpu6502 -> (Int64, Int64)
cpu6502Stmt5MinMaxLength CS5Final{..} = (sz, sz)
  where
    sz = 1 + fromIntegral (length s5fData)
cpu6502Stmt5MinMaxLength CS5Data{..} = (sz, sz)
  where
    sz = fromIntegral (length s5dData)
cpu6502Stmt5MinMaxLength CS5LabelDefinition{} = (0, 0)


--
-- local optimisation
--

cpu6502OptimiseLocalStmtBlockC :: CS5Block Cpu6502 -> CSM4 Cpu6502 (CS5Block Cpu6502)
cpu6502OptimiseLocalStmtBlockC = return . opt5

opt5 :: CS5Block Cpu6502 -> CS5Block Cpu6502
-- jsr sth ; rts -> jmp sth
opt5
  (     CS5Final{s5fCpu = cpuA, s5fCode = [opc|jsr.abs|], ..}
      : CS5Final{s5fCpu = cpuB, s5fCode = [opc|rts.imp|]}
      : block
      )
      = CS5Final{s5fCpu = cpuA `minCpuVariant` cpuB, s5fCode = [opc|jmp.abs|], ..} : opt5 block
-- lda 0 ; ldx 0 -> lax 0 (on 6502i)
opt5
  (     CS5Final{s5fCpu = cpuA, s5fCode = [opc|lda.imm|], s5fData = [E4ConstInt loc 0]}
      : CS5Final{s5fCpu = cpuB, s5fCode = [opc|ldx.imm|], s5fData = [E4ConstInt _ 0]}
      : block
      )
    |   cpuA == cpuVariant6502i &&
        cpuB == cpuVariant6502i
      = CS5Final{s5fCpu = cpuVariant6502i, s5fCode = [opc|lax.imm|], s5fData = [E4ConstInt loc 0]} : opt5 block
-- ldx 0 ; lda 0 -> lax 0 (on 6502i)
opt5
  (     CS5Final{s5fCpu = cpuA, s5fCode = [opc|ldx.imm|], s5fData = [E4ConstInt loc 0]}
      : CS5Final{s5fCpu = cpuB, s5fCode = [opc|lda.imm|], s5fData = [E4ConstInt _ 0]}
      : block
      )
    |   cpuA == cpuVariant6502i &&
        cpuB == cpuVariant6502i
      = CS5Final{s5fCpu = cpuVariant6502i, s5fCode = [opc|lax.imm|], s5fData = [E4ConstInt loc 0]} : opt5 block
-- merge two following data segments together
opt5
  (     CS5Data{s5dData = dataA}
      : CS5Data{s5dData = dataB}
      : block
      )
      = opt5 (CS5Data{s5dData = dataA ++ dataB} : block)
opt5 (x : xs) = x : opt5 xs
opt5 [] = []


--
-- global optimisation
--

cpu6502OptimiseGlobalStmtBlockC :: [CS5Block Cpu6502] -> CSM4 Cpu6502 [CS5Block Cpu6502]
cpu6502OptimiseGlobalStmtBlockC = return
-- TODO: merge [... jmp x] and [x: ...]


--
-- convert to bytes
--

cpu6502StmtBlockToByteValPiece :: CS5Block Cpu6502 -> ByteValPiece (Expr4 Cpu6502)
cpu6502StmtBlockToByteValPiece block =
  ByteValPiece
    { bvpBytes = bytes
    , bvpAlign = cpu6502CodeAlign
    , bvpPage = Nothing
    , bvpNames = names
    }
  where
    (bytes, names) = foldl' toByteValPiece (V.empty, M.empty) block

toByteValPiece :: (Vector (ByteVal (Expr4 Cpu6502)), Map Reference Int64) -> CS5 Cpu6502 -> (Vector (ByteVal (Expr4 Cpu6502)), Map Reference Int64)
toByteValPiece (bytes, names) CS5Final{..} =
  ( bytes
    `V.snoc` byteValWord8 ByteValIsConst (fromIntegral s5fCode)
    ++ V.fromList (map (ByteValCode ByteValIsConst) s5fData)
  , names
  )
toByteValPiece (bytes, names) CS5Data{..} =
  (bytes ++ V.fromList (map (ByteValCode ByteValIsInit) s5dData), names)
toByteValPiece (bytes, names) CS5LabelDefinition{..} =
  (bytes, M.insert s5lLabel (fromIntegral $ V.length bytes) names)
