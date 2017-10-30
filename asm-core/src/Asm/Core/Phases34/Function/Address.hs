module Asm.Core.Phases34.Function.Address where

import           Asm.Core.Prelude

import           Asm.Core.Control.CompilerError
import           Asm.Core.Data.Cpu
import           Asm.Core.Data.FunctionKey
import           Asm.Core.Data.KindDefinition
import           Asm.Core.Data.MetaKey
import           Asm.Core.Data.Reference
import           Asm.Core.Data.TypeDefinition
import           Asm.Core.Flags
import           Asm.Core.Phase3.CompilerState3
import           Asm.Core.Phase3.MetaData
import           Asm.Core.Phase4.Data.Expr4
import           Asm.Core.Phases.Data.PoolDefinition
import           Asm.Core.Phases34.Data.Function
import           Asm.Core.Phases34.Data.PoolState
import           Asm.Core.Phases34.TypeDefinition
import           Asm.Core.SourcePos
import           Asm.Data.InfInt64

data AddrCheck = AddrCheckNone | AddrCheckByte | AddrCheckCode

showAddrCheck :: AddrCheck -> String
showAddrCheck AddrCheckNone = "anything"
showAddrCheck AddrCheckByte = "byte"
showAddrCheck AddrCheckCode = "code"

-- TODO: check if already placed and reduce then
addrC :: (CSM34 m, Cpu c) => AddrCheck -> Function m c
addrC ac loc [(KDPointer t, E4Pointer _ n _ o)] = do
  unless (checkType ac t) $ $throwError [(loc, "Type mismatch, expected: " ++ showAddrCheck ac ++ ", actual: " ++ show t)]
  getPositionC n >>= \case
    (Right p) ->
      return $ FnrResult (KDData TDInt, E4ConstInt loc $ p + o)
    (Left (l, h)) ->
      return $ FnrRangedInt (l + InfInt64 o) (h + InfInt64 o) (Just (n, o))
addrC _ _ [(KDPointer{}, _)] = return $ FnrUnchanged (KDData TDInt)
addrC _ _ _ = return FnrNoMatch


addrDiffC :: (CSM34 m, Cpu c) => Function m c
addrDiffC loc [(KDData TDInt, E4RangedInt _ _ _ (Just (n1,o1)) _), (KDData TDInt, E4RangedInt _ _ _ (Just (n2,o2)) _)]
  | n1 == n2 = return $ FnrResult (KDData TDInt, E4ConstInt loc (o1 - o2))
  | otherwise = return $ FnrUnchanged (KDData TDInt) -- TODO: check if in same pool and distance can be calculated
addrDiffC _ _ = return FnrNoMatch


checkType :: AddrCheck -> TypeDefinition -> Bool
checkType AddrCheckNone _             = True
checkType AddrCheckByte TDByte        = True
checkType AddrCheckByte TDPool{}      = True
checkType AddrCheckByte (TDArray t _) = checkType AddrCheckByte t
checkType AddrCheckByte (TDStruct ts) = all (\(_,t) -> checkType AddrCheckByte t) ts
checkType AddrCheckByte (TDUnion ts)  = all (\(_,t) -> checkType AddrCheckByte t) ts
checkType AddrCheckCode TDCode        = True
checkType _ _                         = False

bankC :: (CSM34 m, Cpu c) => Function m c
bankC loc [(_, E4Pointer _ n _ _)] =
  getPositionPoolC n >>= \case
    Just poolName -> do
      tpd <- $fromJustOrError [(loc, "is not a pool")] =<< getPoolDefinitionC poolName
      return $ FnrResult (KDData TDInt, E4ConstInt loc $ pdBank tpd)
    Nothing ->
      return $ FnrResult (KDData TDInt, E4ConstInt loc 0)
bankC _ _                             = return FnrNoMatch

bankMetaFunctionC :: Cpu c => FunctionKey -> Location -> [Expr4 c] -> CSM3 c (Expr4 c)
bankMetaFunctionC _f loc [] = do
  p <- $fromJustOrError [(loc, "meta.pool.code is not set")] =<< getMetaExprMayC [metaPoolCode]
  (name, _) <- getPoolElemRefC loc p
  tpd <- $fromJustOrError [(loc, "is not a pool"),(locationOf p, "definition of pool")] =<< getPoolDefinitionC name
  return (E4ConstInt loc $ pdBank tpd)
bankMetaFunctionC f loc es  = return (E4Function loc f es)

sizeC :: (CSM34 m, Cpu c) => Function m c
sizeC loc [(_, E4Pointer _ name (TDPool True _ _) _)] = sizeC' loc name
sizeC loc [(_, E4Pointer _ _ TDPool{} _)]
  = $throwFatalError [(loc, "Only the size of a pool definition can be determined, but neither of a pool element nor a variable")]
sizeC loc [(KDPointer t@TDStruct{}, _)]               = sizeC'' loc t
sizeC loc [(KDType t, _)]                             = sizeC'' loc t
sizeC _ _                                             = return FnrNoMatch

sizeC' :: (CSM34 m, Cpu c) => Location -> Reference -> m (FunctionResult c)
sizeC' loc name = do
  sta <- getPoolStateC name
  when flagDebugCompiler $ do
    traceM $ "size" ++ show name ++ " " ++ show (psLengthLow sta) ++ " " ++ show (psLengthHigh sta)
    traceM $ show sta
  if InfInt64 (psLengthLow sta) == psLengthHigh sta
    then return $ FnrResult (KDData TDInt, E4ConstInt loc $ psLengthLow sta)
    else return $ FnrRangedInt (InfInt64 (psLengthLow sta)) (psLengthHigh sta) Nothing

sizeC'' :: (CSM34 m, Cpu c) => Location -> TypeDefinition -> m (FunctionResult c)
sizeC'' loc t = do
  s <- sizeOfTypeDefinitionC t
  return $ FnrResult (KDData TDInt, E4ConstInt loc s)

byteC :: (CSM34 m, Cpu c) => Function m c
byteC loc [ptr]     = asC loc [ptr, (KDType TDByte, E4Type loc TDByte 0)]
byteC loc [ptr,ofs] = asC loc [ptr, (KDType TDByte, E4Type loc TDByte 0),ofs]
byteC _ _           = return FnrNoMatch

codeC :: (CSM34 m, Cpu c) => Function m c
codeC loc [ptr]     = asC loc [ptr, (KDType TDCode, E4Type loc TDCode 0)]
codeC loc [ptr,ofs] = asC loc [ptr, (KDType TDCode, E4Type loc TDCode 0),ofs]
codeC _ _           = return FnrNoMatch

asC :: (CSM34 m, Cpu c) => Function m c
asC _ [(KDPointer{}, E4Pointer loc n _ o), (KDType t, _)] =
  return $ FnrResult (KDPointer t, E4Pointer loc n t o)
asC _ [(KDPointer{}, E4Pointer loc n _ o), (KDType t, _), (KDData TDInt, E4ConstInt _ i)] =
  return $ FnrResult (KDPointer t, E4Pointer loc n t (o+i))

asC _ [(KDPointer{}, _), (KDType t, _)] =
  return $ FnrUnchanged (KDPointer t)
asC _ [(KDPointer{}, _), (KDType t, _), (KDData TDInt, _)] =
  return $ FnrUnchanged (KDPointer t)

asC _ _           = return FnrNoMatch
