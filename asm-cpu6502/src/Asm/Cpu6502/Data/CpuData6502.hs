{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Asm.Cpu6502.Data.CpuData6502
  ( Cpu6502
  , PStmtCpu6502(..)
  , PExprCpu6502
  , CS12Cpu6502(..)
  , CS3Cpu6502(..)
  , CS4Cpu6502(..)
  , CS5Cpu6502(..)
  , PStmt6502
  , PExpr6502
  , PStmtBlock6502
  ) where

import           Asm.Core.Prelude
import           Data.Void

import           Asm.Core.Data.CpuData
import           Asm.Core.Data.Reference
import           Asm.Core.Phase1.Data.Expr12
import           Asm.Core.Phase3.Data.Expr3
import           Asm.Core.Phase4.Data.Expr4
import           Asm.Core.PrettyPrint
import           Asm.Parser.Data.PExpr
import           Asm.Parser.Data.PStmt
import           Asm.Parser.Parser.LabelIdValue

import           Asm.Cpu6502.Data.OpCodes

-- data for this cpu, all is bound to it
data Cpu6502

-- Cpu specific statements/expressions during parsing
data PStmtCpu6502
  = PSRegular
    { s0rOperator    :: Operator
    , s0rIndexMode   :: IndexMode
    , s0rAddressMode :: [AddressMode]
    , s0rExpr        :: Maybe PExpr6502
    }
  | PSInline
    { s0iOperator    :: Operator
    , s0iIndexMode   :: IndexMode
    , s0iAddressMode :: AddressMode
    , s0iName        :: LabelIdValue
    }
  | PSData
    { s0dData :: [PExpr6502]
    }
  deriving (Eq, Data, Typeable)

newtype PExprCpu6502
  = PExprCpu6502 PExprCpu6502
  deriving (Eq, Data, Typeable, Show)

-- CpuData, Phase1+2
data CS12Cpu6502
  = CS1Regular
    { s1rOperator    :: !Operator
    , s1rIndexMode   :: !IndexMode
    , s1rAddressMode :: [AddressMode]
    , s1rExpr        :: Maybe (Expr12 Cpu6502)
    }
  | CS1Inline
    { s1iOperator    :: !Operator
    , s1iIndexMode   :: !IndexMode
    , s1iAddressMode :: !AddressMode
    , s1iName        :: LabelIdValue
    }
  | CS1Data
    { s1dData :: [Expr12 Cpu6502]
    }

-- CpuData, Phase 3
data CS3Cpu6502
  = CS3Regular
    { s3rOperator    :: !Operator
    , s3rIndexMode   :: !IndexMode
    , s3rAddressMode :: [AddressMode]
    , s3rExpr        :: Maybe (Expr3 Cpu6502)
    }
  | CS3Inline
    { s3iOperator    :: !Operator
    , s3iIndexMode   :: !IndexMode
    , s3iAddressMode :: !AddressMode
    , s3iName        :: Reference
    }
  | CS3Data
    { s3dData :: [Expr3 Cpu6502]
    }

-- CpuData, Phase 4
data CS4Cpu6502
  = CS4Inline
    { s4iOperator :: !Operator
    , s4iCode     :: !Word8
    , s4iSize     :: !Int64
    , s4iName     :: Reference
    }
  | CS4Final
    { s4fCpu      :: !CpuVariant
    , s4fOperator :: !Operator
    , s4fCode     :: !Word8
    , s4fData     :: [Expr4 Cpu6502]
    , s4fLabel    :: Maybe Reference
    , s4fOptimise :: Expr4 Cpu6502
    }
  | CS4Data
    { s4dData     :: [Expr4 Cpu6502]
    }
  deriving (Eq, Show)

instance PrettySrc CS4Cpu6502 where
  -- prettySrcM StmtCpu6502{..} = do
  --   let
  --     (aml, amr) =
  --       case sAM of
  --         IMImplied -> ("", "")
  --         IMNone    -> (" ", "")
  --         IMX       -> (" ", ", x")
  --         IMY       -> (" ", ", y")
  --         IMIY      -> (" [", "], y")
  --         IMXI      -> (" [", ", x]")
  --         IMI       -> (" [", "]")
  --     asm' = concatMap (\case
  --         AMAbs  -> ".abs"
  --         AMZp   -> ".zp"
  --         AMImm  -> ".imm"
  --         AMImp  -> ".imp"
  --         AMRel  -> ".rel"
  --       ) (M.keys sASM)
  --   e' <- mapM prettySrcM sExpr
  --   return $ pretty sOperator ++ asm' ++ aml ++ fromMaybe (if sAM == IMImplied then "" else "?expr") e' ++ amr
  prettySrcM x = return $ pshow x

-- CpuData, Phase 5
data CS5Cpu6502
  = CS5Final
    { s5fCpu  :: !CpuVariant
    , s5fCode :: !Word8
    , s5fData :: [Expr4 Cpu6502]
    }
  | CS5Data
    { s5dData :: [Expr4 Cpu6502]
    }
  | CS5LabelDefinition
    { s5lLabel :: Reference
    }
  deriving (Eq, Show)

instance PrettySrc CS5Cpu6502 where
  prettySrcM x = return $ pshow x

-- CpuData type
instance CpuData Cpu6502 where
  -- statements as described above
  type CS12 Cpu6502 = CS12Cpu6502
  type CS3 Cpu6502 = CS3Cpu6502
  type CS4 Cpu6502 = CS4Cpu6502
  type CS5 Cpu6502 = CS5Cpu6502
  -- we don't have any expressions
  type CE12 Cpu6502 = Void
  type CE3 Cpu6502 = Void
  type CE4 Cpu6502 = Void

instance Show PStmtCpu6502 where
  show (PSRegular o a s e) = "OP " ++ "." ++ showPretty o ++ "." ++ show a ++ "/" ++ show s ++ " " ++ maybe "-" Asm.Parser.Data.PExpr.dumpExpr e
  show (PSInline o a s e) = "OP " ++ "." ++ showPretty o ++ "." ++ show a ++ "/" ++ show s ++ " " ++ show e
  show (PSData e) = "DATA " ++ show (map Asm.Parser.Data.PExpr.dumpExpr e)


-- shortcuts for parser data
type PStmt6502 = PStmt PStmtCpu6502 PExprCpu6502
type PExpr6502 = PExpr PExprCpu6502
type PStmtBlock6502 = [PStmt PStmtCpu6502 PExprCpu6502]
