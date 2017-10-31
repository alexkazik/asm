module Asm.Cpu6502.Compiler.Phase3
  ( cpu6502ApplyMetaStmtC
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

import {-# SOURCE #-} Asm.Cpu6502.Data.Cpu6502         ()
import           Asm.Cpu6502.Data.CpuData6502
import           Asm.Cpu6502.Data.FunctionKey
import           Asm.Cpu6502.Data.MetaKey
import           Asm.Cpu6502.Data.OpCodes

--
-- convert phase 3 to 4: store meta data where needed
--

cpu6502ApplyMetaStmtC :: Location -> CS3 Cpu6502 -> CSM3 Cpu6502 (Stmt4Block Cpu6502)
cpu6502ApplyMetaStmtC loc CS3Data{..} = recoverFatalError [] $ do
  s4dData' <- mapM applyMetaExprC s3dData
  sdCheck8 <- getCheck8C loc [metaCheckImm8, metaCheckImm, metaCheck]
  let
    s4dData = flip map s4dData' $ \expr ->
      E4Function loc sdCheck8 [expr]
  return [S4CpuStmt loc CS4Data{..}]

cpu6502ApplyMetaStmtC loc CS3Regular{s3rExpr = Nothing, ..} = do
  (s4fCpu, am) <- pickCpu loc s3rOperator s3rIndexMode
  let
    op = M.filterWithKey (\k _ -> k `elem` s3rAddressMode) am
    s4fOperator = s3rOperator
    s4fData = []
    s4fLabel = Nothing
  s4fOptimise <- $fromJustOrError [(loc, "meta.optimise is undefined")] =<< (map (\(_,a,_) -> a) <$> getMetaExprMayC [metaOptimise])
  case M.lookup AMImp op of
    Just (s4fCode, _, _) ->
      return [S4CpuStmt loc CS4Final{..}]
      -- return [S4CpuStmt loc $ CS4Final c s3rOperator code [] Nothing sfOptimise]
    _ -> $throwFatalError [(loc, "unknown addressing mode for opcode " ++ showPretty s3rOperator)]

cpu6502ApplyMetaStmtC loc CS3Regular{s3rExpr = Just s3rExpr, ..} = do
  (s4fCpu, am') <- pickCpu loc s3rOperator s3rIndexMode
  srExprMeta <- applyMetaExprC s3rExpr
  (srExpr', am) <- addressModeOfExpression loc s3rOperator srExprMeta am'
  s4fOptimise <- $fromJustOrError [(loc, "meta.optimise is undefined")] =<< (map (\(_,a,_) -> a) <$> getMetaExprMayC [metaOptimise])
  let
    op = M.filterWithKey (\k _ -> k `elem` s3rAddressMode) am
    s4fOperator = s3rOperator
    s4fLabel = Nothing

  op' <- forM (M.toAscList op) $ \case
    (AMImm, (s4fCode, checkMay, _)) -> do
      checkDef <- getCheck8C loc [metaCheckImm8, metaCheckImm, metaCheck]
      let
        s4fData = [ E4Function loc (fromMaybe checkDef checkMay) [srExpr'] ]
      return (AMImm, Nothing, [ S4CpuStmt loc CS4Final{..} ] )
    (AMZp, (s4fCode, checkMay, Just fnAddrType)) -> do
      let
        addr = E4Function loc fnAddrType [srExpr']
        s4fData = [ E4Function loc (fromMaybe fnCheckAddr8 checkMay) [addr] ]
      return (AMZp, Nothing, [ S4CpuStmt loc CS4Final{..} ] )
    (AMAbs, (s4fCode, checkMay, Just fnAddrType)) -> do
      let
        addr = E4Function loc fnAddrType [srExpr']
        s4fData =
          [ E4Function loc (fromMaybe fnCheckAddr16 checkMay) [addr]
          , E4Function loc opShiftR [addr, E4ConstInt loc 8]
          ]
      return (AMAbs, Just addr, [ S4CpuStmt loc CS4Final{..} ] )
    (AMRel, (s4fCode, checkMay, _)) -> do
      nid <- addNameC (pack $ showPretty s3rOperator)
      setPositionC nid (Nothing, Left (0, maxBound))
      let
        s4fData =
          [ E4Function loc (fromMaybe fnConvertRel8 checkMay)
              [ E4Function loc fnAddrCode [srExpr']
              , E4Function loc fnAddrCode [E4Pointer loc nid TDCode 0]
              ]
          ]
      return
        ( AMRel
        , Nothing
        , [ S4CpuStmt loc CS4Final{..}
          , S4LabelDefinition loc nid
          ]
        )
    x -> error $ show (x, s3rOperator, s3rIndexMode, s3rAddressMode, loc)
  case op' of
    [(_, _, stmt)] -> return stmt
    [(AMAbs, Just addrAbs, stmtAbs), (AMZp, _, stmtZp)] ->
                return
                  [ S4IfBlock loc
                    [ ( "~~"
                      , E4Function loc opLE
                        [ addrAbs
                        , E4ConstInt loc 0xff
                        ]
                      , stmtZp
                      )
                    , ( "~~"
                      , E4ConstBool loc True
                      , stmtAbs
                      )
                    ]
                  ]
    x              -> error $ show (map (\(a,_,_) -> a) x, s3rOperator, s3rIndexMode, s3rAddressMode, loc)

cpu6502ApplyMetaStmtC loc CS3Inline{..} = do
  (_, am) <- pickCpu loc s3iOperator s3iIndexMode
  (s4iCode, _, _) <- $fromJustOrError [(loc, "unknown addressing mode for opcode " ++ showPretty s3iOperator)] (M.lookup s3iAddressMode am)
  let
    s4iOperator = s3iOperator
    s4iName = s3iName
  s4iSize <- sizeOfStmt s3iAddressMode
  return [ S4CpuStmt loc CS4Inline{..} ]
  where
    sizeOfStmt AMAbs = return 2
    sizeOfStmt AMZp  = return 1
    sizeOfStmt AMImm = return 1
    sizeOfStmt AMRel = return 1
    sizeOfStmt _     = $throwFatalError [(loc, "Unable to determine size of statement")]


-- helper

addressModeOfExpression :: Pretty a => Location -> Operator -> Expr4 Cpu6502 -> Map AddressMode a -> CSM3 Cpu6502 (Expr4 Cpu6502, Map AddressMode a)
addressModeOfExpression loc sOperator sExpr sOp = do
  (typeOfExpr, expr') <- evaluateExprTopC sExpr
  let
    addrmodeOfExpr =
      case typeOfExpr of
        KDPointer{} -> [AMZp, AMAbs, AMRel]
        KDData{}    -> [AMImm]
        _           -> []
    s3 = M.filterWithKey (\k _ -> k `elem` addrmodeOfExpr) sOp
  when (null s3) $ $throwFatalError [(loc, "unknown addressing mode for opcode(3) " ++ showPretty sOperator ++ "; t2: " ++ showPretty typeOfExpr ++ "; sOp: " ++ showPretty sOp)]
  return (expr', s3)

pickCpu :: Location -> Operator -> IndexMode -> CSM3 Cpu6502 (CpuVariant, Map AddressMode (Word8, Maybe FunctionKey, Maybe FunctionKey))
pickCpu loc sOperator sAM = do
  (c, cloc) <- $fromJustOrError [(loc, "meta.cpu is not set")] =<< getMetaMagicMayC [metaCpu]
  cpu <- $fromJustOrError [(loc, "unknown cpu " ++ show c),(cloc, "defined at")] (M.lookup c cpuVariants)
  ops <- $fromJustOrError [(loc, "unknown cpu " ++ show c),(cloc, "defined at")] (IM.lookup (fromCpuVariant cpu) opcodes)
  op <- $fromJustOrError [(loc, "unknown opcode " ++ showPretty sOperator)] (IM.lookup (fromOperator sOperator) ops)
  op' <- $fromJustOrError [(loc, "unknown IndexMode " ++ showPretty sOperator)] (M.lookup sAM op)
  return (cpu, op')
