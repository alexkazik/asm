module Asm.Core.Prelude
  ( module Export
  , ltshow
  ) where

import           ClassyPrelude            as Export hiding (Builder (..), mask, terror, try, undefined, union)
import           Control.Monad.State.Lazy as Export (MonadState, State, evalState, get, put, runState, state)
import           Data.Bits                as Export
import           Data.Char                as Export (ord)
import           Data.Data                as Export (Data)
import           Data.Int                 as Export (Int16, Int8)
import           Data.List                as Export (tails)
import           Data.Ratio               as Export
import           Data.Word                as Export (Word16, Word8)
import           Prelude                  as Export (errorWithoutStackTrace, undefined)

ltshow :: Show a => a -> LText
ltshow = tlshow
