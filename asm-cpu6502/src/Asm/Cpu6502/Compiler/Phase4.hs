module Asm.Cpu6502.Compiler.Phase4
  ( cpu6502StmtMinMaxLength
  , cpu6502ConvertToStmt5C
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Control.CompilerError
import           Asm.Core.Data.CpuData
import           Asm.Core.Data.Ternary
import           Asm.Core.Phase4.CompilerState4
import           Asm.Core.Phase4.Data.Expr4
import           Asm.Core.Phases34.EvaluateExpr
import           Asm.Core.SourcePos

import {-# SOURCE #-} Asm.Cpu6502.Data.Cpu6502       ()
import           Asm.Cpu6502.Data.CpuData6502
import           Asm.Cpu6502.Data.FunctionKey

--
-- general
--

cpu6502StmtMinMaxLength :: CS4 Cpu6502 -> (Int64, Int64)
cpu6502StmtMinMaxLength CS4Inline{..} = (sz, sz)
  where
    sz = 1 + s4iSize
cpu6502StmtMinMaxLength CS4Final{..} = (sz, sz)
  where
    sz = 1 + fromIntegral (length s4fData)
cpu6502StmtMinMaxLength CS4Data{..} = (sz, sz)
  where
    sz = fromIntegral (length s4dData)


--
-- convert from phase 4 to 5
--

cpu6502ConvertToStmt5C :: Location -> CS4 Cpu6502 -> CSM4 Cpu6502 (CS5Block Cpu6502)
cpu6502ConvertToStmt5C loc CS4Inline{..} =
  getInlineC s4iName >>= \case
    Just (size, ee) -> do
      e3 <- case ee of
        Just eee -> snd <$> evaluateExprTopC eee
        Nothing  -> return (E4ConstMaskedInt loc (0 *& 0))
      when (size /= s4iSize) $ $throwError [(loc, "inline variable has the wrong size")]
      setHasChangedC
      return
        [ CS5Data
            { s5dData = [E4ConstInt loc (fromIntegral s4iCode)]
            }
        , CS5LabelDefinition s4iName
        , CS5Data
            { s5dData = genData e3 s4iSize []
            }
        ]
    Nothing -> do
      $throwError [(loc, "can't find inline variable")]
      return []
    where
      genData _ 0 res    = res
      genData expr n res = expr : genData (E4Function loc opShiftR [expr, E4ConstInt loc 8]) (n-1) res
cpu6502ConvertToStmt5C _ CS4Final{..} = return [CS5Final{s5fCpu = s4fCpu, s5fCode = s4fCode, s5fData = s4fData}]
cpu6502ConvertToStmt5C _ CS4Data{..} = return [CS5Data{s5dData = s4dData}]
