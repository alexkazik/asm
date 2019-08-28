{-# LANGUAGE NoImplicitPrelude #-}

module Asm.Parser.DirectBlock
  ( pickDirectBlock
  ) where

import           Asm.Core.Prelude

import           Asm.Core.SourcePos

import           Asm.Parser.Data.PStmt

pickDirectBlock :: SourcePos -> [(Bool, [PStmt ps pe])] -> PStmt ps pe
pickDirectBlock loc ((True, a):_)  = (loc, PSNamespace Nothing a)
pickDirectBlock loc ((False, _):a) = pickDirectBlock loc a
pickDirectBlock loc []             = (loc, PSNamespace Nothing [])
