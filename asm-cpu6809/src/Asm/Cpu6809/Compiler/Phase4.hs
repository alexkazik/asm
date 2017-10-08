module Asm.Cpu6809.Compiler.Phase4 where

import           Asm.Core.Prelude

import           Asm.Core.Data.CpuData
import           Asm.Core.Data.Ternary
import           Asm.Core.Phase4.CompilerState4
import           Asm.Core.Phase4.Data.Expr4
import           Asm.Core.Phases34.EvaluateExpr
import           Asm.Core.SourcePos

import {-# SOURCE #-} Asm.Cpu6809.Data.Cpu6809       ()
import           Asm.Cpu6809.Data.CpuData6809
import           Asm.Cpu6809.Data.FunctionKey

cpu6809ConvertToStmt5C :: Location -> CS4 Cpu6809 -> CSM4 Cpu6809 (CS5Block Cpu6809)
cpu6809ConvertToStmt5C loc CS4Inline{..} =
  getInlineC s4iInline >>= \case
    Just (size, ee) -> do
      e3 <- case ee of
        Just eee -> snd <$> evaluateExprTopC eee
        Nothing  -> return (E4ConstMaskedInt loc (0 *& 0))
      when (size /= s4iSize) $ printErrorC $ (loc, "inline variable has the wrong size"):[sourcePos||]
      setHasChangedC
      return
        [ CS5Data
            { s5dExpr = operatorCodes loc s4iCode
            }
        , CS5LabelDefinition
            { s5lLabel = s4iInline
            }
        , CS5Data
            { s5dExpr = genData e3 size []
            }
        ]
    Nothing -> printErrorC $ (loc, "can't find inline variable"):[sourcePos||]
    where
      genData _ 0 res    = res
      genData expr n res = genData (E4Function loc opShiftR [expr, E4ConstInt loc 8]) (n-1) res ++ [expr]
cpu6809ConvertToStmt5C loc CS4Final{..} = do
  optExpr <- snd <$> evaluateExprTopC s4fOptimise
  let
    opt =
      case optExpr of
        E4ConstBool _ v -> v
        _               -> printError $ (loc, "meta.optimise has to be flat"):[sourcePos||]
  s5fData <- mapM (fmap snd . evaluateExprTopC) s4fData
  return $
    bool
      [ CS5Data
          { s5dExpr = (operatorCodes loc s4fCode) ++ s5fData
          }
      ]
      [ CS5Final
          { s5fCode = s4fCode
          , ..
          }
      ]
      opt
cpu6809ConvertToStmt5C _ CS4Data{..} = return [CS5Data{s5dExpr = s4dExpr}]

cpu6809StmtMinMaxLength :: CS4 Cpu6809 -> (Int64, Int64)
cpu6809StmtMinMaxLength CS4Inline{..} = (sz, sz)
  where
    sz = bool 1 2 (s4iCode >= 0xff) + s4iSize
cpu6809StmtMinMaxLength CS4Final{..} = (sz, sz)
  where
    sz = bool 1 2 (s4fCode >= 0xff) + fromIntegral (length s4fData)
cpu6809StmtMinMaxLength CS4Data{..} = (1, 1)

operatorCodes :: Location -> Word16 -> [Expr4 Cpu6809]
{-# INLINE operatorCodes #-}
operatorCodes loc code
  | code > 0xff =
    [ E4ConstInt loc (fromIntegral code `shiftR` 8)
    , E4ConstInt loc (fromIntegral code)
    ]
  | otherwise =
    [ E4ConstInt loc (fromIntegral code)
    ]
