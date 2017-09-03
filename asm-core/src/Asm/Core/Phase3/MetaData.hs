module Asm.Core.Phase3.MetaData where

import           Asm.Core.Prelude

import           Asm.Core.Data.Cpu
import           Asm.Core.Data.KindDefinition
import           Asm.Core.Data.MetaKey
import           Asm.Core.Data.Reference
import           Asm.Core.Data.TypeDefinition
import           Asm.Core.Phase3.CompilerState3
import           Asm.Core.Phase4.Data.Expr4
import           Asm.Core.PrettyPrint.Use
import           Asm.Core.SourcePos

getPoolElemRefC :: Cpu c => Location -> (KindDefinition, Expr4 c, Location) -> CSM3 c (Reference, Bool)
getPoolElemRefC _ (_, E4Pointer _ pn (TDPool _ True virt) 0, _) = return (pn, virt)
getPoolElemRefC loc (k,e, loc2)             = printErrorC $ (loc, "it's not a pool: " ++ showPretty k ++ " <- " ++ showPrettySrc e):(loc2, "definition"):[sourcePos||]


{-
  Get/Set all metas (within each block at first get is called then
  the block is executed and at the end the metas are resored).
-}

getAllMetaC :: Cpu c => CSM3 c (MetaKeyMap (KindDefinition, Expr4 c, Location))
getAllMetaC = state (\s -> (cs3MetaData s, s))

setAllMetaC :: Cpu c => MetaKeyMap (KindDefinition, Expr4 c, Location) -> CSM3 c ()
setAllMetaC meta = state (\s -> ((), s{cs3MetaData = meta}))

getAllMetaStickyC :: Cpu c => CSM3 c MetaKeySet
getAllMetaStickyC = state (\s -> (cs3MetaStickyData s, s))

setAllMetaStickyC :: Cpu c => MetaKeySet -> CSM3 c ()
setAllMetaStickyC meta = state (\s -> ((), s{cs3MetaStickyData = meta}))

{-
  Get/Set one information, set is called by the handler of "#", get within the applyMeta functions
-}

-- getMetaC :: Cpu c => [MetaKey] -> CSM3 c (Maybe (Either (Expr4 c) Text, Location))
-- getMetaC keys = do
--   meta <- getAllMetaC
--   return $ listToMaybe $ catMaybes $ map (flip M.lookup meta) keys

getMetaMagicMayC :: Cpu c => [MetaKey] -> CSM3 c (Maybe (Text, Location))
getMetaMagicMayC keys'' = do
  meta <- getAllMetaC
  go meta keys''
  where
    go _ [] = return Nothing
    go meta (key:keys') =
      case mkmLookup key meta of
        Just (_, E4MagicValue _ v, loc) -> return $ Just (v, loc)
        Just (_, _, loc)                -> printErrorC $ (loc, "meta is expected to be a magic value"):[sourcePos||]
        Nothing                         -> go meta keys'


getMetaExprMayC :: Cpu c => [MetaKey] -> CSM3 c (Maybe (KindDefinition, Expr4 c, Location))
getMetaExprMayC keys'' = do
  meta <- getAllMetaC
  go meta keys''
  where
    go _ [] = return Nothing
    go meta (key:keys') =
      case mkmLookup key meta of
        Just v  -> return $ Just v
        Nothing -> go meta keys'

setMetaExprC :: Cpu c => MetaKey -> (KindDefinition, Expr4 c) -> Location -> CSM3 c ()
setMetaExprC key (kind, value) loc = do
  meta <- getAllMetaC
  setAllMetaC $ mkmInsert key (kind, value, loc) meta

unsetMetaExprC :: Cpu c => MetaKey -> CSM3 c ()
unsetMetaExprC key = do
  meta <- getAllMetaC
  setAllMetaC $ mkmDelete key meta

stickyMetaExprC :: Cpu c => MetaKey -> CSM3 c ()
stickyMetaExprC key = do
  meta <- getAllMetaStickyC
  setAllMetaStickyC $ mksInsert key meta

unstickyMetaExprC :: Cpu c => MetaKey -> CSM3 c ()
unstickyMetaExprC key = do
  meta <- getAllMetaStickyC
  setAllMetaStickyC $ mksDelete key meta
