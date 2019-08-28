{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Asm.Cpu6809.Data.CpuData6809
  ( Cpu6809
  , PStmtCpu6809(..)
  , PExprCpu6809
  , CS12Cpu6809(..)
  , CS3Cpu6809(..)
  , CS4Cpu6809(..)
  , CS5Cpu6809(..)
  , PStmt6809
  , PExpr6809
  , PStmtBlock6809
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

import           Asm.Cpu6809.Data.OpCodes

-- data for this cpu, all is bound to it
data Cpu6809

-- Cpu specific statements/expressions during parsing
data PStmtCpu6809
  = PSInline
    { s0iOperator :: Operator
    , s0iAM       :: AddressMode
    , s0iIW       :: Maybe Int
    , s0iInline   :: LabelIdValue
    }
  | PSIndexed
    { s0xOperator :: Operator
    , s0xIndexed  :: IndexedMode
    , s0xExpr     :: Maybe PExpr6809
    , s0xIndirect :: Bool
    }
  | PSRegular
    { s0rOperator :: Operator
    , s0rAM       :: [AddressMode]
    , s0rExpr     :: Maybe PExpr6809
    }
  | PSData
    { s0dExpr     :: [PExpr6809]
    }
  deriving (Eq, Data, Typeable, Show)

newtype PExprCpu6809
  = PExprCpu6809 PExprCpu6809
  deriving (Eq, Data, Typeable, Show)

-- CpuData, Phase1+2
data CS12Cpu6809
  = CS1Inline
    { s1iOperator :: !Operator
    , s1iAM       :: !AddressMode
    , s1iIW       :: Maybe Int
    , s1iInline   :: LabelIdValue
    }
  | CS1Indexed
    { s1xOperator :: !Operator
    , s1xIndexed  :: !IndexedMode
    , s1xExpr     :: Maybe (Expr12 Cpu6809)
    , s1xIndirect :: !Bool
    }
  | CS1Regular
    { s1rOperator :: !Operator
    , s1rAM       :: [AddressMode]
    , s1rExpr     :: Maybe (Expr12 Cpu6809)
    }
  | CS1Data
    { s1dExpr     :: [Expr12 Cpu6809]
    }

-- CpuData, Phase 3
data CS3Cpu6809
  = CS3Inline
    { s3iOperator :: !Operator
    , s3iAM       :: !AddressMode
    , s3iIW       :: Maybe Int
    , s3iInline   :: Reference
    }
  | CS3Indexed
    { s3xOperator :: !Operator
    , s3xIndexed  :: !IndexedMode
    , s3xExpr     :: Maybe (Expr3 Cpu6809)
    , s3xIndirect :: !Bool
    }
  | CS3Regular
    { s3rOperator :: !Operator
    , s3rAM       :: [AddressMode]
    , s3rExpr     :: Maybe (Expr3 Cpu6809)
    }
  | CS3Data
    { s3dExpr     :: [Expr3 Cpu6809]
    }

-- CpuData, Phase 4
data CS4Cpu6809
  = CS4Inline
    { s4iOperator :: !Operator
    , s4iCode     :: !Word16
    , s4iSize     :: !Int64
    , s4iInline   :: Reference
    }
  | CS4Final
    { s4fOperator :: !Operator
    , s4fCode     :: !Word16
    , s4fData     :: [Expr4 Cpu6809]
    , s4fInline   :: Maybe Reference
    , s4fOptimise :: Expr4 Cpu6809
    }
  | CS4Data
    { s4dExpr     :: [Expr4 Cpu6809]
    }
  deriving (Eq, Show)

instance PrettySrc CS4Cpu6809 where
  prettySrcM x = return $ pshow x

-- CpuData, Phase 5
data CS5Cpu6809
  -- can be optimised
  = CS5Final
    { s5fCode :: !Word16
    , s5fData :: [Expr4 Cpu6809]
    }
  -- data, inline or should not optimised
  | CS5Data
    { s5dExpr   :: [Expr4 Cpu6809]
    }
  | CS5LabelDefinition
    { s5lLabel :: Reference
    }
  deriving (Eq, Show)

instance PrettySrc CS5Cpu6809 where
  prettySrcM x = return $ pshow x

-- CpuData type
instance CpuData Cpu6809 where
  -- statements as described above
  type CS12 Cpu6809 = CS12Cpu6809
  type CS3 Cpu6809 = CS3Cpu6809
  type CS4 Cpu6809 = CS4Cpu6809
  type CS5 Cpu6809 = CS5Cpu6809
  -- we don't have any expressions
  type CE12 Cpu6809 = Void
  type CE3 Cpu6809 = Void
  type CE4 Cpu6809 = Void

-- shortcuts for parser data
type PStmt6809 = PStmt PStmtCpu6809 PExprCpu6809
type PExpr6809 = PExpr PExprCpu6809
type PStmtBlock6809 = [PStmt PStmtCpu6809 PExprCpu6809]
