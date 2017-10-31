module Asm.Core.Phase4.CompilerState4
  ( module Asm.Core.Phase4.Data.CompilerState4
  , module Asm.Core.Phases34.Data.CompilerState34
  , module Asm.Core.Phases.Data.CompilerState1234
  , initialReader4
  , initialState4
  , getUniqueNameC
  , resetLoopDataC
  , resetLoopDataS
  , setDefaultC
  , getHasChangedC
  , getHighestDefaultC
  , getUseDefaultC
  , resetUseDefaultC
  , getMetaIsFlatC
  , setMetaIsFlatC
  , getInlineC
  ) where

import           Asm.Core.Prelude
import qualified Data.Map.Strict                        as M

import           Asm.Core.Data.Cpu
import           Asm.Core.Data.Reference
import           Asm.Core.Phase3.Data.CompilerState3
import           Asm.Core.Phase4.Data.CompilerState4
import           Asm.Core.Phase4.Data.Expr4
import           Asm.Core.Phases.Data.CompilerState1234
import           Asm.Core.Phases34.Data.CompilerState34
import           Asm.Core.Phases34.Function

initialReader4 :: Cpu c => CompilerReader3 c -> CompilerState3 c -> CompilerWriter3 c -> CompilerReader4 c
initialReader4 CRd3{..} CSt3{..} CWr3{..} =
  CRd4
    { cs4PoolDefinition = cs3PoolDefinition
    , cs4Functions = functionKeyMap
    }

initialState4 :: Cpu c => CompilerReader3 c -> CompilerState3 c -> CompilerWriter3 c -> CompilerState4 c
initialState4 CRd3{..} CSt3{..} CWr3{..} =
  CSt4
    { cs4UniqueNumber = 10000
    , cs4Position = cs3Position
    , cs4Inline = cs3Inline
    , cs4PoolData = cs3PoolData
    , cs4PoolState = cs3PoolState
    , cs4HighestDefault = Nothing
    , cs4UseDefault = Nothing
    , cs4HasChanged = False
    , cs4MetaIsFlat = False
    }

getUniqueNameC :: Cpu c => CSM4 c Text
getUniqueNameC = do
  s@CSt4{..} <- get
  put s{cs4UniqueNumber = cs4UniqueNumber + 1}
  return ('~' `cons` tshow cs4UniqueNumber)

resetLoopDataC :: Cpu c => Maybe (Ratio Int) -> CSM4 c ()
resetLoopDataC def = modify (resetLoopDataS def)

resetLoopDataS :: Cpu c => Maybe (Ratio Int) -> CompilerState4 c -> CompilerState4 c
resetLoopDataS def s = s{cs4HighestDefault = Nothing, cs4UseDefault = def, cs4HasChanged = False, cs4MetaIsFlat = False}

setDefaultC :: Cpu c => Ratio Int -> CSM4 c ()
setDefaultC def = modify (\s -> if Just def > cs4HighestDefault s then s{cs4HighestDefault = Just def} else s)

getHasChangedC :: Cpu c => CSM4 c Bool
getHasChangedC = gets cs4HasChanged

getHighestDefaultC :: Cpu c => CSM4 c (Maybe (Ratio Int))
getHighestDefaultC = gets cs4HighestDefault

getUseDefaultC :: Cpu c => CSM4 c (Maybe (Ratio Int))
getUseDefaultC = gets cs4UseDefault

resetUseDefaultC :: Cpu c => CSM4 c ()
resetUseDefaultC = modify (\s -> s{cs4UseDefault = Nothing})

getMetaIsFlatC :: Cpu c => CSM4 c Bool
getMetaIsFlatC = gets cs4MetaIsFlat

setMetaIsFlatC :: Cpu c => Bool -> CSM4 c ()
setMetaIsFlatC isFlat = modify (\s -> s{cs4MetaIsFlat = isFlat})

getInlineC :: Cpu c => Reference -> CSM4 c (Maybe (Int64, Maybe (Expr4 c)))
getInlineC n = do
  s@CSt4{..} <- get
  case M.lookup n cs4Inline of
    Nothing -> return Nothing
    i@Just{} -> do
      put s{cs4Inline = M.delete n cs4Inline}
      return i
