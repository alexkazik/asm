{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Asm.Core.Phase4.Compiler
  ( compile4
  ) where

import           Asm.Core.Prelude
import qualified Data.Map                            as M
import           System.IO.Unsafe

import           Asm.Core.Control.CompilerError
import           Asm.Core.Data.Cpu
import           Asm.Core.Data.Reference
import           Asm.Core.Flags
import           Asm.Core.Phase3.CompilerState3
import           Asm.Core.Phase4.CompilerState4
import           Asm.Core.Phase4.Data.CompilerResult
import           Asm.Core.Phase4.Pool
import           Asm.Core.Phase4.PoolData
import           Asm.Core.Phase4.PoolState

compile4 :: Cpu c => (CompilerReader3 c, CompilerState3 c, CompilerWriter3 c) -> Error CompilerError (CompilerResult c)
compile4 (r3, s3, w3) = do
  let
    r4 = initialReader4 r3 s3 w3
  ((), s4, ()) <- runRWST go r4 (initialState4 r3 s3 w3)
  let
    result = map (getFinalPoolsS s4) (M.toList $ cs4PoolDefinition r4)
  return $ CompilerResult (map (\(a, b, c, d) -> (nameOfReference a, b, c, d)) result) s4
  where
    go = do
      outerLoopC Nothing
      when flagDebugCompiler (outerLoopC Nothing) -- in case of debugging: do another loop and check that nothing changes


outerLoopC :: Cpu c => Maybe (Ratio Int) -> CSM4 c ()
outerLoopC work = do
  -- debug compiler
  when flagDebugCompiler $ do
    un <- getUniqueNameC
    traceM $ "[OuterLoop:" ++ unpack un ++ "  " ++ show work ++ "]"
  -- /debug compiler
  loopC work
  work' <- getHighestDefaultC
  allFinal <- areAllPoolsFinalC
  if | allFinal -> return ()
     | isJust work' -> outerLoopC work'
     | otherwise -> $throwFatalError [([], "can't reduce it")]

loopC :: Cpu c => Maybe (Ratio Int) -> CSM4 c ()
loopC work = do
  resetLoopDataC work
  -- debug compiler
  debugIn <- bool (return Nothing) (do
      un <- getUniqueNameC
      traceM $ "[Loop:" ++ unpack un ++ "  " ++ show work ++ "]"
      s1 <- get -- DEBUG: state on input
      return (Just (un, s1))
    ) flagDebugCompiler
  -- /debug compiler
  promoteErrorToFatalError $ do
    pools <- getPoolsC
    pools' <- M.fromAscList <$> mapM reducePoolDataStateC (M.toAscList pools)
    setPoolsC pools'
    calcPoolStateC
  hasChanged <- getHasChangedC
  -- debug compiler
  when flagDebugCompiler $ forM_ debugIn $ \(un, s1) -> do
    s2 <- resetLoopDataS work <$> get -- DEBUG: state on output
    when (hasChanged /= (s1 /= s2)) $ -- DEBUG: when hasChanges does not reflect a real change
      error $ unsafePerformIO $ do
        writeFile "/tmp/asm.sta.in" ("[Loop Mismatch IN]\n" ++ encodeUtf8 (toStrict $ dumpStateS s1) ++ "\n")
        writeFile "/tmp/asm.sta.out" ("[Loop Mismatch OUT]\n" ++ encodeUtf8 (toStrict $ dumpStateS s2) ++ "\n")
        return $ "Loop Mismatch: " ++ unpack un ++ "; hasChanged=" ++ show hasChanged ++ "; data different=" ++ show (s1 /= s2)
  -- /debug compiler
  when hasChanged $ loopC Nothing
