module Asm.Core.Data.VariableType
  ( VariableType(..)
  ) where

import           Asm.Core.Prelude

data VariableType
  = VTVar
  | VTConst
  | VTLocal
  | VTInline
  | VTPointer
  deriving (Data, Eq, Show, Typeable)
