module Asm.Core.Phase4.Data.PoolData where

import           Asm.Core.Prelude
import qualified Data.Set                   as S
import qualified Data.Vector                as V

import           Asm.Core.Data.ByteVal
import           Asm.Core.Data.ByteValPiece
import           Asm.Core.Data.CpuData
import           Asm.Core.Data.Reference
import           Asm.Core.Phase4.Data.Expr4
import           Asm.Core.Phase4.Data.Stmt4
import           Asm.Core.PrettyPrint
import           Asm.Core.SourcePos

data PoolData c
  = PoolDataStartFlat
    { stateStart :: ![Stmt4Block c]
    , stateFlat  :: ![CS5Block c]
    , stateData  :: !(Set (ByteValPiece (Expr4 c)))
    , statePool  :: ![(Reference, Reference, ConstOrInit)]
    }
  | PoolDataOptimised
    { stateOpt   :: !(Set (ByteValPiece (Expr4 c)))
    , stateFinal :: !(Set (ByteValPiece (Expr4 c)))
    }
  | PoolDataFinal !(ByteValPiece (Expr4 c))
  deriving Eq

addBlockToStartPool :: Stmt4Block c -> PoolData c -> PoolData c
addBlockToStartPool b ps@PoolDataStartFlat {stateStart} = ps{stateStart = b : stateStart}
addBlockToStartPool _ _                                 = printError [sourcePos|addBlockToStartPool: try to add afterwards|]

addVariableToDataPool :: CpuData c => ByteValPiece (Expr4 c) -> PoolData c -> PoolData c
addVariableToDataPool b ps@PoolDataStartFlat {stateData} = ps{stateData = S.insert b stateData}
addVariableToDataPool _ _                                = printError [sourcePos|addVariableToDataPool: try to add afterwards|]

addPoolToPoolPoolC :: (Reference, Reference, ConstOrInit) -> PoolData c -> PoolData c
addPoolToPoolPoolC b ps@PoolDataStartFlat {statePool} = ps{statePool = b : statePool}
addPoolToPoolPoolC _ _                                = printError [sourcePos|addPoolToPoolPoolC: try to add afterwards|]

getByteValVector :: PoolData c -> Vector (ByteVal (Expr4 c))
getByteValVector (PoolDataFinal bvp) = bvpBytes bvp
getByteValVector _                   = printError [sourcePos|getByteValVector: the data it not yet final|]


instance CpuData c => Pretty (PoolData c) where
  pretty PoolDataStartFlat{stateStart, stateFlat, stateData, statePool} =
    align $ vsep
      [ dumpPoolData' "start:start" stateStart
      , dumpPoolData' "start:flat" stateFlat
      , dumpPoolData'' "start:data" (S.toList stateData)
      , dumpPoolData''' "start:pool" statePool
      ]

  pretty PoolDataOptimised{stateOpt, stateFinal} =
    align $ vsep
      [ dumpPoolData'' "optimised:start" (S.toList stateOpt)
      , dumpPoolData'' "optimised:final" (S.toList stateFinal)
      ]

  pretty (PoolDataFinal p) =
    dumpPoolData'' "FINAL" [p]

dumpPoolData' :: PrettySrc a => Text -> [[a]] -> Doc
dumpPoolData' _ [] = ""
dumpPoolData' pool blocks = align $ vsep $ map
  ( \block ->
      pretty pool <+> align (dumpStmtBlock block)
  ) blocks

dumpPoolData'' :: PrettySrc e => Text -> [ByteValPiece e] -> Doc
dumpPoolData'' _ [] = ""
dumpPoolData'' pool blocks = pretty pool <+> align (vsep $ map sub blocks)
  where
    sub ByteValPiece{bvpBytes, bvpAlign, bvpPage, bvpNames} =
      "BVP" <+> align
        ( vsep
          [ "align:" <+> pretty bvpAlign <+> "; page:" <+> pretty bvpPage
          , fillList $ map pretty (V.toList bvpBytes)
          , pretty bvpNames
          ]
        )

dumpPoolData''' :: Text -> [(Reference, Reference, ConstOrInit)] -> Doc
dumpPoolData''' _ []        = ""
dumpPoolData''' pool blocks = vsep ["pool" <+> pretty pool, pshow blocks]
