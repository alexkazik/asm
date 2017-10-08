module Asm.Cpu6809.Compiler.Phase5 where

import           Asm.Core.Prelude
import qualified Data.Map.Strict                as M
import qualified Data.Vector                    as V

import           Asm.Core.Data.ByteVal
import           Asm.Core.Data.ByteValPiece
import           Asm.Core.Data.CpuData
import           Asm.Core.Data.Reference
import           Asm.Core.Phase4.CompilerState4
import           Asm.Core.Phase4.Data.Expr4

import           Asm.Cpu6809.Data.CpuData6809
import           Asm.Cpu6809.OpCodes.Quote

cpu6809CodeAlign :: Int64
cpu6809CodeAlign = 1

cpu6809OptimiseLocalStmtBlockC :: CS5Block Cpu6809 -> CSM4 Cpu6809 (CS5Block Cpu6809)
cpu6809OptimiseLocalStmtBlockC = return . go
  where
    go :: CS5Block Cpu6809 -> CS5Block Cpu6809
    -- jsr sth ; rts -> jmp sth
    go
      (     CS5Final{s5fCode = [opc|jsr.dir|], ..}
          : CS5Final{s5fCode = [opc|rts.imp|]}
          : block
          )
          = CS5Final{s5fCode = [opc|jmp.dir|], ..} : go block
    go
      (     CS5Final{s5fCode = [opc|jsr.idx|], ..}
          : CS5Final{s5fCode = [opc|rts.imp|]}
          : block
          )
          = CS5Final{s5fCode = [opc|jmp.idx|], ..} : go block
    go
      (     CS5Final{s5fCode = [opc|jsr.ext|], ..}
          : CS5Final{s5fCode = [opc|rts.imp|]}
          : block
          )
          = CS5Final{s5fCode = [opc|jmp.ext|], ..} : go block
    -- puls (w/o PC) ; rts -> puls (w PC)
    go
      (     CS5Final{s5fCode = [opc|puls.imm|], s5fData = [E4ConstInt loc regs]}
          : CS5Final{s5fCode = [opc|rts.imp|]}
          : block
          )
        |   regs .&. 0x80 == 0
          = CS5Final{s5fCode = [opc|puls.imm|], s5fData = [E4ConstInt loc (regs .|. 0x80)]} : go block
    -- puls PC -> rts
    go
      (     CS5Final{s5fCode = [opc|puls.imm|], s5fData = [E4ConstInt _ 0x80]}
          : block
          )
          = CS5Final{s5fCode = [opc|rts.imp|], s5fData = []} : go block
    -- lda.imm ; ldb.imm -> ldd.imm
    go
      (     CS5Final{s5fCode = [opc|lda.imm|], s5fData = dataA}
          : CS5Final{s5fCode = [opc|ldb.imm|], s5fData = dataB}
          : block
          )
          = CS5Final{s5fCode = [opc|ldd.imm|], s5fData = dataA ++ dataB} : go block
    -- ldb.imm ; lda.imm -> ldd.imm
    go
      (     CS5Final{s5fCode = [opc|ldb.imm|], s5fData = dataB}
          : CS5Final{s5fCode = [opc|lda.imm|], s5fData = dataA}
          : block
          )
          = CS5Final{s5fCode = [opc|ldd.imm|], s5fData = dataA ++ dataB} : go block
    -- merge two following data segments together
    go
      (     CS5Data{s5dExpr = dataA}
          : CS5Data{s5dExpr = dataB}
          : block
          )
          = go (CS5Data{s5dExpr = dataA ++ dataB} : block)
    -- not to optimise statement: skip it, try the rest
    go (stmt : block) = stmt : go block
    -- done
    go [] = []


cpu6809OptimiseGlobalStmtBlockC :: [CS5Block Cpu6809] -> CSM4 Cpu6809 [CS5Block Cpu6809]
cpu6809OptimiseGlobalStmtBlockC = return
-- TODO: merge [... jmp x] and [x: ...]

cpu6809StmtBlockToByteValPiece :: CS5Block Cpu6809 -> ByteValPiece (Expr4 Cpu6809)
cpu6809StmtBlockToByteValPiece block =
  ByteValPiece
    { bvpBytes = bytes
    , bvpAlign = cpu6809CodeAlign
    , bvpPage = Nothing
    , bvpNames = names
    }
  where
    (bytes, names) = foldl' toByteValPiece (V.empty, M.empty) block


toByteValPiece :: (Vector (ByteVal (Expr4 Cpu6809)), Map Reference Int64) -> CS5 Cpu6809 -> (Vector (ByteVal (Expr4 Cpu6809)), Map Reference Int64)
toByteValPiece (bytes, names) CS5Final{..}
  | s5fCode <= 0xff =
    ( bytes ++ V.fromList
      ( byteValWord8 ByteValIsConst (fromIntegral s5fCode)
      : map (ByteValCode ByteValIsConst) s5fData
      )
    , names
    )
  | otherwise =
    ( bytes ++ V.fromList
      ( byteValWord8 ByteValIsConst (fromIntegral (s5fCode `shiftR` 8))
      : byteValWord8 ByteValIsConst (fromIntegral s5fCode)
      : map (ByteValCode ByteValIsConst) s5fData
      )
    , names
    )

toByteValPiece (bytes, names) CS5Data{..} = (bytes ++ V.fromList (map (ByteValCode ByteValIsInit) s5dExpr), names)

toByteValPiece (bytes, names) CS5LabelDefinition{..} = (bytes, M.insert s5lLabel (fromIntegral $ V.length bytes) names)

cpu6809Stmt5MinMaxLength :: CS5 Cpu6809 -> (Int64, Int64)
cpu6809Stmt5MinMaxLength CS5Final{..} = (sz, sz)
  where
    sz = bool 1 2 (s5fCode >= 0xff) + fromIntegral (length s5fData)
cpu6809Stmt5MinMaxLength CS5Data{..} = (sz, sz)
  where
    sz = fromIntegral (length s5dExpr)
cpu6809Stmt5MinMaxLength CS5LabelDefinition{} = (0, 0)
