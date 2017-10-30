module Asm.Core.Phases34.TypeDefinition
  ( sizeOfTypeDefinitionC
  ) where

import           Asm.Core.Prelude

import           Asm.Core.Control.CompilerError
import           Asm.Core.Data.TypeDefinition
import           Asm.Core.Phases34.Data.CompilerState34


sizeOfTypeDefinitionC :: CSM34 m => TypeDefinition -> m Int64

sizeOfTypeDefinitionC TDByte = return 1

sizeOfTypeDefinitionC (TDTypeRef loc ref) = $throwFatalError [(loc, "typeref not resolved: " ++ show ref)]

sizeOfTypeDefinitionC (TDArray _ Nothing) =
  return 0

sizeOfTypeDefinitionC (TDArray t (Just l)) = do
  size <- sizeOfTypeDefinitionC t
  return $ l * size

sizeOfTypeDefinitionC (TDStruct l) = foldM soStructElem 0 l
  where
    soStructElem s (_,t) = (+) s <$> sizeOfTypeDefinitionC t

sizeOfTypeDefinitionC (TDUnion l) = do
  sizes <- mapM (sizeOfTypeDefinitionC . snd) l
  return $ maximumEx sizes

sizeOfTypeDefinitionC x = $throwFatalError [([], "sizeOfTypeDefinitionC " ++ show x)]
