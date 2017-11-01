module Asm.Cpu6809.Compiler.Phase3
  ( cpu6809ApplyMetaStmtC
  ) where

import           Asm.Core.Prelude
import qualified Data.IntMap.Strict               as IM
import qualified Data.Map.Strict                  as M

import           Asm.Core.Control.CompilerError
import           Asm.Core.Data.CpuData
import           Asm.Core.Data.KindDefinition
import           Asm.Core.Data.TypeDefinition
import           Asm.Core.Phase3.CompilerState3
import           Asm.Core.Phase3.MetaDataApply
import           Asm.Core.Phase4.Data.Expr4
import           Asm.Core.Phase4.Data.Stmt4
import           Asm.Core.Phases34.EvaluateExpr
import           Asm.Core.Phases34.Function.Check
import           Asm.Core.PrettyPrint.Use
import           Asm.Core.SourcePos

import {-# SOURCE #-} Asm.Cpu6809.Data.Cpu6809         ()
import           Asm.Cpu6809.Data.CpuData6809
import           Asm.Cpu6809.Data.FunctionKey
import           Asm.Cpu6809.Data.MetaKey
import           Asm.Cpu6809.Data.OpCodes

cpu6809ApplyMetaStmtC :: Location -> CS3 Cpu6809 -> CSM3 Cpu6809 (Stmt4Block Cpu6809)
cpu6809ApplyMetaStmtC loc CS3Inline{..} = recoverFatalError [] $ do
  (_, am) <- pickCpu loc s3iOperator
  op@(code, _, _) <- $fromJustOrError [(loc, "unknown addressing mode for opcode " ++ showPretty s3iOperator)] (M.lookup s3iAM am)
  return
    [ S4CpuStmt
        loc
        CS4Inline
          { s4iOperator = s3iOperator
          , s4iCode = code
          , s4iSize = sizeOfStmt s3iAM s3iIW op
          , s4iInline = s3iInline
          }
    ]
  where
    sizeOfStmt AMImm _ (_, imm16, _) = bool 1 2 imm16
    sizeOfStmt AMDir _ _             = 1
    sizeOfStmt AMIdx (Just 1) _      = 1
    sizeOfStmt AMIdx (Just 2) _      = 2
    sizeOfStmt AMIdx (Just 3) _      = 3
    sizeOfStmt AMExt _ _             = 2
    sizeOfStmt AMRel _ (_, rel16, _) = bool 1 2 rel16
    sizeOfStmt _ _ _                 = $printError [(loc, "Unable to determine size of statement")]

cpu6809ApplyMetaStmtC loc CS3Indexed{..} = do
  (_c, am) <- pickCpu loc s3xOperator
  exprMay <- mapM applyMetaExprC s3xExpr
  sxOp' <- $fromJustOrError [(loc, "unknown addressing mode for opcode " ++ showPretty s3xOperator)] (M.lookup AMIdx am)
  checkOfs16 <- getCheck16C loc [metaCheckOfs16, metaCheckOfs, metaCheck]
  s4fOptimise <- $fromJustOrError [(loc, "meta.optimise is undefined")] =<< (map (\(_,a,_) -> a) <$> getMetaExprMayC [metaOptimise])
  let
    sxOp = bool sxOp' ((\(a,b,_) -> (a,b,Just fnAddrByte)) sxOp') s3xIndirect
  case (s3xIndexed, exprMay, sxOp) of
    (IMIndirect, Just expr, (s4fCode, _, Just fnAddrType)) -> do
      return
        [ S4CpuStmt
            loc
            CS4Final
              { s4fOperator = s3xOperator
              , s4fData =
                  [ E4ConstInt loc 0b10011111
                  , E4Function loc opShiftR [E4Function loc fnAddrType [expr], E4ConstInt loc 8]
                  , E4Function loc fnCheckAddr16 [E4Function loc fnAddrType [expr]]
                  ]
              , s4fInline = Nothing
              , ..
              }
        ]
    (IMRelOffset RegPC, Just expr, (s4fCode, _, Just fnAddrType)) -> do
      nid <- addNameC (pack $ showPretty s3xOperator)
      setPositionC nid (Nothing, Left (0, maxBound))
      let
        im = indirectMask s3xIndirect
        relAddr = [E4Function loc fnAddrType [expr], E4Function loc fnAddr [E4Pointer loc nid TDCode 0]]
        relAddr8 = E4Function loc fnConvertRel8 relAddr
        relAddr16 = E4Function loc fnConvertRel16 relAddr
      return
        [ S4IfBlock loc
          [ ( "~~"
            , E4Function loc fnCheckRelIs8 relAddr
            , [ S4CpuStmt loc
                  CS4Final
                    { s4fOperator = s3xOperator
                    , s4fData =
                        [ E4ConstInt loc $ 0b10001100 .|. im
                        , relAddr8
                        ]
                    , s4fInline = Nothing
                    , ..
                    }
              ]
            )
          , ( "~~"
            , E4ConstBool loc True
            , [ S4CpuStmt loc
                  CS4Final
                    { s4fOperator = s3xOperator
                    , s4fData =
                        [ E4ConstInt loc $ 0b10001101 .|. im
                        , E4Function loc opShiftR [relAddr16, E4ConstInt loc 8]
                        , relAddr16
                        ]
                    , s4fInline = Nothing
                    , ..
                    }
              ]
            )
          ]
        , S4LabelDefinition loc nid
        ]

    (IMRelOffset r, Just expr, (s4fCode, _, Just fnAddrType)) -> do
      let
        (regName, regMeta) =
          case r of
            RegX -> ("meta.x", metaX)
            RegY -> ("meta.y", metaY)
            RegU -> ("meta.u", metaU)
            RegS -> ("meta.s", metaS)
            _    -> $printError [(loc, "invalid register")]
      registerValue <- $fromJustOrError [(loc, regName ++ " is undefined")] =<< (map (\(_,a,_) -> a) <$> getMetaExprMayC [regMeta])
      let
        offset =
          E4Function loc opMINUS
            [ E4Function loc fnAddrType [expr]
            , registerValue
            ]
      genIMOffset loc checkOfs16 s3xOperator s3xIndirect s4fOptimise r s4fCode offset
    (IMOffset r, _, (s4fCode, _, _)) -> do
      offset <- evaluateExprTopC (fromMaybe (E4ConstInt loc 0) exprMay) >>= \case
        (KDData{}, expr) -> return expr
        _ -> $throwFatalError [(loc, "unknown addressing mode for opcode(5) " ++ showPretty s3xOperator)]
      genIMOffset loc checkOfs16 s3xOperator s3xIndirect s4fOptimise r s4fCode offset
    (IMIncrement r bwd dbl, _, (s4fCode, _, _)) -> do
      rm <- ofsRegBit loc r
      let
        im = indirectMask s3xIndirect
        incdec =
          case (bwd, dbl) of
            (False, False) -> 0b00000000
            (False, True ) -> 0b00000001
            (True,  False) -> 0b00000010
            (True,  True ) -> 0b00000011
      offset <- evaluateExprTopC (fromMaybe (E4ConstInt loc 0) exprMay) >>= \case
        (KDData{}, expr) -> return expr
        _ -> $throwFatalError [(loc, "unknown addressing mode for opcode(5) " ++ showPretty s3xOperator)]
      return
        [ S4IfBlock loc
          [ ( "~~"
            , E4Function loc fnCheckOffsetZero
              [ offset
              ]
            , [ S4CpuStmt loc
                  CS4Final
                    { s4fOperator = s3xOperator
                    , s4fData = [E4ConstInt loc $ 0b10000000 .|. rm .|. im .|. incdec ]
                    , s4fInline = Nothing
                    , ..
                    }
              ]
            )
          ]
        ]
    _ -> $throwFatalError [(loc, "unknown addressing mode for opcode(6) " ++ showPretty s3xOperator)]

cpu6809ApplyMetaStmtC loc CS3Regular{s3rExpr = Nothing, ..} = do
  (_c, am) <- pickCpu loc s3rOperator
  let
    op = M.filterWithKey (\k _ -> k `elem` s3rAM) am
  s4fOptimise <- $fromJustOrError [(loc, "meta.optimise is undefined")] =<< (map (\(_,a,_) -> a) <$> getMetaExprMayC [metaOptimise])
  case M.toList op of
    [(AMImp, (s4fCode, _, _))] ->
      return
        [
          S4CpuStmt
            loc
            CS4Final
              { s4fOperator = s3rOperator
              , s4fData = []
              , s4fInline = Nothing
              , ..
              }
        ]
    _ -> $throwFatalError [(loc, "unknown addressing mode for opcode " ++ showPretty s3rOperator)]

cpu6809ApplyMetaStmtC loc CS3Regular{s3rExpr = Just s3rExpr, ..} = do
  (_c, am') <- pickCpu loc s3rOperator
  srDp <- map (\(_,a,_) -> a) <$> getMetaExprMayC [metaDP]
  s3rExpr' <- applyMetaExprC s3rExpr
  (srExpr', am) <- addressModeOfExpression loc s3rOperator s3rExpr' am'
  s4fOptimise <- $fromJustOrError [(loc, "meta.optimise is undefined")] =<< (map (\(_,a,_) -> a) <$> getMetaExprMayC [metaOptimise])
  let
    op = M.filterWithKey (\k _ -> k `elem` s3rAM) am
    genExt :: Word16 -> FunctionKey -> Stmt4Block Cpu6809
    genExt s4fCode fnAddrType =
        [ S4CpuStmt loc
            CS4Final
              { s4fOperator = s3rOperator
              , s4fData =
                  [ E4Function loc opShiftR [E4Function loc fnAddrType [srExpr'], E4ConstInt loc 8]
                  , E4Function loc fnCheckAddr16 [E4Function loc fnAddrType [srExpr']]
                  ]
              , s4fInline = Nothing
              , ..
              }
        ]

  case M.toAscList op of
    [(AMRel, (s4fCode, rel16, _))] -> do
      nid <- addNameC (pack $ showPretty s3rOperator)
      setPositionC nid (Nothing, Left (0, maxBound))
      let
        relAddr check = E4Function loc check [E4Function loc fnAddrCode [srExpr'], E4Function loc fnAddrCode [E4Pointer loc nid TDCode 0]]
      if rel16
        then
          return
            [ S4CpuStmt loc
                CS4Final
                  { s4fOperator = s3rOperator
                  , s4fData =
                      [ E4Function loc opShiftR [relAddr fnConvertRel16, E4ConstInt loc 8]
                      , relAddr fnConvertRel16
                      ]
                  , s4fInline = Nothing
                  , ..
                  }
            , S4LabelDefinition loc nid
            ]
        else
          return
            [ S4CpuStmt loc
                CS4Final
                  { s4fOperator = s3rOperator
                  , s4fData =
                      [ relAddr fnConvertRel8
                      ]
                  , s4fInline = Nothing
                  , ..
                  }
            , S4LabelDefinition loc nid
            ]

    [(AMImm, (s4fCode, imm16, _))] -> do
      if imm16
        then do
          check <- getCheck8C loc [metaCheckImm16, metaCheckImm, metaCheck]
          return
            [ S4CpuStmt loc
                CS4Final
                  { s4fOperator = s3rOperator
                  , s4fData =
                      [ E4Function loc opShiftR [srExpr', E4ConstInt loc 8]
                      , E4Function loc check [srExpr']
                      ]
                  , s4fInline = Nothing
                  , ..
                  }
            ]
        else do
          check <- getCheck8C loc [metaCheckImm8, metaCheckImm, metaCheck]
          return
            [ S4CpuStmt loc
                CS4Final
                  { s4fOperator = s3rOperator
                  , s4fData =
                      [ E4Function loc check [srExpr']
                      ]
                  , s4fInline = Nothing
                  , ..
                  }
            ]
    [(AMDir, (s4fCode, _, Just fnAddrType))] -> return $
            [ S4CpuStmt loc
                CS4Final
                  { s4fOperator = s3rOperator
                  , s4fData =
                      [ E4Function loc fnCheckAddr8 [E4Function loc fnAddrType [srExpr']]
                      ]
                  , s4fInline = Nothing
                  , ..
                  }
            ]

    [(AMExt, (code, _, Just fnAddrType))] -> return $ genExt code fnAddrType
    [(AMDir, (s4fCode, _, _)), (AMExt, (codeExt, _, Just fnAddrType))] ->
            case srDp of
              Just dp ->
                return
                  [ S4IfBlock loc
                    [ ( "~~"
                      , E4Function loc opEQ
                        [ E4Function loc opShiftR [E4Function loc fnAddrType [srExpr'], E4ConstInt loc 8]
                        , dp
                        ]
                      , [ S4CpuStmt loc
                            CS4Final
                              { s4fOperator = s3rOperator
                              , s4fData =
                                  [ E4Function loc fnAddrType [srExpr']
                                  ]
                              , s4fInline = Nothing
                              , ..
                              }
                        ]
                      )
                    , ( "~~"
                      , E4ConstBool loc True
                      , genExt codeExt fnAddrType
                      )
                    ]
                  ]
              Nothing ->
                return $ genExt codeExt fnAddrType
    _ -> $throwFatalError [(loc, "unknown addressing mode for opcode " ++ showPretty s3rOperator)]
cpu6809ApplyMetaStmtC loc CS3Data{..} = do
  sdExprMeta <- mapM applyMetaExprC s3dExpr
  sdCheck8 <- getCheck8C loc [metaCheckImm8, metaCheckImm, metaCheck]
  let
    s4dExpr = flip map sdExprMeta $ \expr ->
      E4Function loc sdCheck8 [expr]
  return [S4CpuStmt loc CS4Data{..}]

pickCpu :: Location -> Operator -> CSM3 Cpu6809 (Text, Map AddressMode (Word16, Bool, Maybe FunctionKey))
pickCpu loc sOperator = do
  (c, cloc) <- $fromJustOrError [(loc, "meta.cpu is not set")] =<< getMetaMagicMayC [metaCpu]
  cpu <- $fromJustOrError [(loc, "unknown cpu " ++ show c),(cloc, "defined at")] (M.lookup c cpuVariants)
  ops <- $fromJustOrError [(loc, "unknown cpu " ++ show c),(cloc, "defined at")] (IM.lookup (fromCpuVariant cpu) opcodes)
  op <- $fromJustOrError [(loc, "unknown opcode " ++ showPretty sOperator)] (IM.lookup (fromOperator sOperator) ops)
  return (c, op)

addressModeOfExpression :: Pretty a => Location -> Operator -> Expr4 Cpu6809 -> Map AddressMode a -> CSM3 Cpu6809 (Expr4 Cpu6809, Map AddressMode a)
addressModeOfExpression loc sOperator sExpr sOp = do
  (typeOfExpr, expr') <- evaluateExprTopC sExpr
  let
    addrmodeOfExpr =
      case typeOfExpr of
        KDPointer{} -> [AMDir, AMExt, AMRel]
        KDData{}    -> [AMImm]
        _           -> []
    s3 = M.filterWithKey (\k _ -> k `elem` addrmodeOfExpr) sOp
  when (null s3) $ $throwFatalError [(loc, "unknown addressing mode for opcode(3) " ++ showPretty sOperator ++ "; t2: " ++ showPretty typeOfExpr ++ "; sOp: " ++ showPretty sOp)]
  return (expr', s3)


isInRange :: Location -> Int64 -> Expr4 Cpu6809 -> Int64 -> Expr4 Cpu6809
isInRange loc lo e hi =
  E4Function loc opLAND
    [ E4Function loc opGE [e, E4ConstInt loc lo]
    , E4Function loc opLE [e, E4ConstInt loc hi]
    ]

ofsRegBit :: Location -> Register -> CSM3 Cpu6809 Int64
ofsRegBit _ RegX = return 0b00000000
ofsRegBit _ RegY = return 0b00100000
ofsRegBit _ RegU = return 0b01000000
ofsRegBit _ RegS = return 0b01100000
ofsRegBit loc _  = $throwFatalError [(loc, "invalid register")]

indirectMask :: Bool -> Int64
indirectMask = bool 0 0b00010000

genIMOffset
  :: Location
  -> FunctionKey
  -> Operator
  -> Bool
  -> Expr4 Cpu6809
  -> Register
  -> Word16
  -> Expr4 Cpu6809
  -> CSM3 Cpu6809 [Stmt4 Cpu6809]
genIMOffset loc checkOfs16 s3xOperator s3xIndirect s4fOptimise r s4fCode offset = do
  let
    isPC = r == RegPC
    im = indirectMask s3xIndirect
  rm <- bool (ofsRegBit loc r) (return 0b00000100) isPC
  return
    [ S4IfBlock loc
      [ ( "~~"
        , bool
          ( E4Function loc opEQ
            [ offset
            , E4ConstInt loc 0
            ]
          )
          (E4ConstBool loc False)
          isPC
        , [ S4CpuStmt loc
              CS4Final
                { s4fOperator = s3xOperator
                , s4fData = [E4ConstInt loc $ 0b10000100 .|. rm .|. im]
                , s4fInline = Nothing
                , ..
                }
          ]
        )
      , ( "~~"
        , bool (isInRange loc (-16) offset 15) (E4ConstBool loc False) (s3xIndirect || isPC)
        , [ S4CpuStmt loc
              CS4Final
                { s4fOperator = s3xOperator
                , s4fData =
                  [ E4Function loc opOR
                    [ E4ConstInt loc rm
                    , E4Function loc opAND
                      [ offset
                      , E4ConstInt loc 0x1f
                      ]
                    ]
                  ]
                , s4fInline = Nothing
                , ..
                }
          ]
        )
      , ( "~~"
        , isInRange loc (-128) offset 127
        , [ S4CpuStmt loc
              CS4Final
                { s4fOperator = s3xOperator
                , s4fData =
                  [ E4ConstInt loc $ 0b10001000 .|. im .|. rm
                  , offset
                  ]
                , s4fInline = Nothing
                , ..
                }
          ]
        )
      , ( "~~"
        , E4ConstBool loc True
        , [ S4CpuStmt loc
              CS4Final
                { s4fOperator = s3xOperator
                , s4fData =
                  [ E4ConstInt loc $ 0b10001001 .|. im .|. rm
                  , E4Function loc opShiftR [offset, E4ConstInt loc 8]
                  , E4Function loc checkOfs16 [offset]
                  ]
                , s4fInline = Nothing
                , ..
                }
          ]
        )
      ]
    ]
