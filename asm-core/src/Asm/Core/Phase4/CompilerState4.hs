module Asm.Core.Phase4.CompilerState4
  ( module Asm.Core.Phase4.Data.CompilerState4
  , module Asm.Core.Phase4.CompilerState4
  , module Asm.Core.Phases34.Data.CompilerState34
  , module Asm.Core.Phases.Data.CompilerState1234
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
getUniqueNameC = state getUniqueNameC'
  where
    getUniqueNameC' s@CSt4 {cs4UniqueNumber} = ('~' `cons` tshow cs4UniqueNumber, s{cs4UniqueNumber=cs4UniqueNumber+1} )

resetLoopDataC :: Cpu c => Maybe (Ratio Int) -> CSM4 c ()
resetLoopDataC def = state (\s -> ((), resetLoopDataS def s))

resetLoopDataS :: Cpu c => Maybe (Ratio Int) -> CompilerState4 c -> CompilerState4 c
resetLoopDataS def s = s{cs4HighestDefault = Nothing, cs4UseDefault = def, cs4HasChanged = False, cs4MetaIsFlat = False}

setDefaultC :: Cpu c => Ratio Int -> CSM4 c ()
setDefaultC def = state (\s -> ((), if Just def > cs4HighestDefault s then s{cs4HighestDefault = Just def} else s))

getHasChangedC :: Cpu c => CSM4 c Bool
getHasChangedC = state (\s -> (cs4HasChanged s, s))

getHighestDefaultC :: Cpu c => CSM4 c (Maybe (Ratio Int))
getHighestDefaultC = state (\s -> (cs4HighestDefault s, s))

getUseDefaultC :: Cpu c => CSM4 c (Maybe (Ratio Int))
getUseDefaultC = state (\s -> (cs4UseDefault s, s))

resetUseDefaultC :: Cpu c => CSM4 c ()
resetUseDefaultC = state (\s -> ((), s{cs4UseDefault = Nothing}))

getMetaIsFlatC :: Cpu c => CSM4 c Bool
getMetaIsFlatC = state (\s -> (cs4MetaIsFlat s, s))

setMetaIsFlatC :: Cpu c => Bool -> CSM4 c ()
setMetaIsFlatC isFlat = state (\s -> ((), s{cs4MetaIsFlat = isFlat}))

getInlineC :: Cpu c => Reference -> CSM4 c (Maybe (Int64, Maybe (Expr4 c)))
getInlineC n = state (\s -> (M.lookup n $ cs4Inline s, s{cs4Inline = M.delete n $ cs4Inline s}))
