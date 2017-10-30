{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Asm.Cpu6809
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
import           Language.Haskell.TH.Quote

import           Asm.Core
import           Asm.Parser

import           Asm.Cpu6809.Data.CpuData6809
import           Asm.Cpu6809.Suffixed

-- external names

type Asm = Asm6809
type Expr = Expr6809

-- parser
expr :: QuasiQuoter
expr = expr6809

asm :: QuasiQuoter
asm = asm6809

asmFile :: FilePath -> TH.ExpQ
asmFile = asmFile6809

-- ToByte specialized to this cpu

type ToByte e = ToByte6809 e

byte :: TH.Q TH.Exp
byte = byte6809

-- ToArray specialized to this cpu

type ToArray e = ToArray6809 e

array :: TH.Q TH.Exp
array = array6809

-- ToStructOrUnion specialized to this cpu

type ToStructOrUnion e = ToStructOrUnion6809 e

structOrUnion :: TH.Q TH.Exp
structOrUnion = structOrUnion6809

-- specialized versions of structOrUnion

struct :: TH.Q TH.Exp
struct = struct6809

union :: TH.Q TH.Exp
union = union6809

-- the compile function

compile :: Asm -> CompilerResult Cpu6809
compile = compile6809
