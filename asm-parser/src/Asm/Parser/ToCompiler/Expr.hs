module Asm.Parser.ToCompiler.Expr
  ( convertExpr
  ) where

import           Asm.Core.Prelude
import qualified Data.Vector                   as V

import           Asm.Core.Phase1.Data.Expr12
import           Asm.Core.SourcePos

import           Asm.Parser.Data.PExpr
import           Asm.Parser.Parser.Tools
import           Asm.Parser.ToCompiler.PCState

convertExpr :: CpuParser c ps pe => PExpr pe -> PCSM (Expr12 c)
convertExpr (loc, PEUserStructOrUnion str) = do
  loc' <- getPathPC loc
  str' <- mapM convertExpr str
  return (E12UserStructOrUnion loc' str' Nothing)
convertExpr (loc, PEByteVal bv) = do
  loc' <- getPathPC loc
  bv' <- mapM convertExpr bv
  return (E12ByteVal loc' bv')

convertExpr (loc, PEUserArray arr) = do
  loc' <- getPathPC loc
  arr' <- mapM convertExpr arr
  return (E12UserArray loc' arr' Nothing)
convertExpr (loc, PEUserArrayL arr) = do
  loc' <- getPathPC loc
  arr' <- mapM convertExpr arr
  return (E12UserArray loc' (V.fromList arr') Nothing)
convertExpr (loc, PEUserArrayBVS arr) = do
  loc' <- getPathPC loc
  return (E12UserArrayBVS loc' arr Nothing)
convertExpr (loc, PEDerefArray a e) = do
  loc' <- getPathPC loc
  a' <- convertExpr a
  e' <- convertExpr e
  return (E12DerefArray loc' a' e')
convertExpr (loc, PEDefineArray a e) = do
  loc' <- getPathPC loc
  a' <- convertExpr a
  e' <- mapM convertExpr e
  return (E12DefineArray loc' a' e')
convertExpr (loc, PEDerefStruct a e) = do
  loc' <- getPathPC loc
  a' <- convertExpr a
  let e' = labelIdValue loc' e
  return (E12DerefStruct loc' a' e')

convertExpr (loc, PEUOperator f a) = do
  loc' <- getPathPC loc
  a' <- convertExpr a
  return (E12Function loc' f [a'])
convertExpr (loc, PEBOperator f a b) = do
  loc' <- getPathPC loc
  a' <- convertExpr a
  b' <- convertExpr b
  return (E12Function loc' f [a',b'])
convertExpr (loc, PEFunction f as) = do
  loc' <- getPathPC loc
  as' <- mapM convertExpr as
  let f' = labelIdValue loc' f
  return (E12Function loc' f' as')
convertExpr (loc, PELabelId l) = do
  loc' <- getPathPC loc
  let l' = labelIdValue loc' l
  return (E12LabelRef loc' l')

convertExpr (loc, PEConstInt c) = do
  loc' <- getPathPC loc
  return (E12ConstInt loc' c)
convertExpr (loc, PEConstMaskedInt c) = do
  loc' <- getPathPC loc
  return (E12ConstMaskedInt loc' c)
convertExpr (loc, PEConstBool c) = do
  loc' <- getPathPC loc
  return (E12ConstBool loc' c)

convertExpr (loc, PETypeStruct str) = do
  loc' <- getPathPC loc
  str' <- mapM (convertParsedStringFstAndExprSnd loc') str
  return (E12TypeStruct loc' str')
convertExpr (loc, PETypeUnion str) = do
  loc' <- getPathPC loc
  str' <- mapM (convertParsedStringFstAndExprSnd loc') str
  return (E12TypeUnion loc' str')

convertExpr (loc, PETraceStep e) = do
  pushPathPC loc
  e' <- convertExpr e
  popPathPC
  return e'

convertExpr (loc, PECpuExpr e) = do
  loc' <- getPathPC loc
  e' <- convertCpuExpr loc' e
  return (E12CpuExpr loc' e')

convertExpr (loc, PEMagicValue v) = do
  loc' <- getPathPC loc
  return (E12MagicValue loc' v)

convertExpr (loc, x@PEAntiExpr{}) = printError $ ([loc], "convertExpr: " ++ show x):[sourcePos||]
convertExpr (loc, x@PEAntiArray{}) = printError $ ([loc], "convertExpr: " ++ show x):[sourcePos||]
convertExpr (loc, x@PEAntiStruct{}) = printError $ ([loc], "convertExpr: " ++ show x):[sourcePos||]

convertParsedStringFstAndExprSnd :: CpuParser c ps pe => Location -> (Maybe LabelIdValue, PExpr pe) -> PCSM (Maybe Text, Expr12 c)
convertParsedStringFstAndExprSnd loc (a, e) = do
  let
    a' = map (labelIdValue loc) a
  e' <- convertExpr e
  return (a', e')
