{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Asm.Cpu6502
  ( module Asm.Core
  , module Asm.Parser
  , Asm
  , asm
  , asmFile
  , Expr
  , expr
  , ToByte
  , byte
  , ToArray
  , array
  , ToStructOrUnion
  , structOrUnion
  , struct
  , union
  , compile
  ) where

import           Asm.Core.Prelude
import qualified Language.Haskell.TH          as TH
import           Language.Haskell.TH.Quote    (QuasiQuoter)

import           Asm.Core
import           Asm.Parser

import           Asm.Cpu6502.Data.CpuData6502
import           Asm.Cpu6502.Suffixed

-- external names

type Asm = Asm6502
type Expr = Expr6502

-- parser
expr :: QuasiQuoter
expr = expr6502

asm :: QuasiQuoter
asm = asm6502

asmFile :: FilePath -> TH.ExpQ
asmFile = asmFile6502

-- ToByte specialized to this cpu

type ToByte e = ToByte6502 e

byte :: TH.Q TH.Exp
byte = byte6502

-- ToArray specialized to this cpu

type ToArray e = ToArray6502 e

array :: TH.Q TH.Exp
array = array6502

-- ToStructOrUnion specialized to this cpu

type ToStructOrUnion e = ToStructOrUnion6502 e

structOrUnion :: TH.Q TH.Exp
structOrUnion = structOrUnion6502

-- specialized versions of structOrUnion

struct :: TH.Q TH.Exp
struct = struct6502

union :: TH.Q TH.Exp
union = union6502

-- the compile function

compile :: Asm -> CompilerResult Cpu6502
compile = compile6502
