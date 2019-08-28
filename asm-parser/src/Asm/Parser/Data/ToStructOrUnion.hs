{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Asm.Parser.Data.ToStructOrUnion
  ( ToStructOrUnion(..)
  ) where

import           Asm.Core.Prelude
import qualified Data.Map.Strict         as M

import           Asm.Core.SourcePos

import           Asm.Parser.Data.PExpr
import           Asm.Parser.Data.ToExpr
import           Asm.Parser.Parser.Tools


class ToStructOrUnion pe a where
  toStructOrUnion :: CpuParser c ps pe => SourcePos -> a -> PExpr pe

instance ToExpr pe a => ToStructOrUnion pe (Map Text a) where
  toStructOrUnion loc a = (loc, PEUserStructOrUnion $ M.map (toExpr loc) a)

instance ToExpr pe a => ToStructOrUnion pe (Map String a) where
  toStructOrUnion loc a = (loc, PEUserStructOrUnion $ M.fromList $ map (pack *** toExpr loc) $ M.toList a)

instance ToExpr pe a => ToStructOrUnion pe [(Text, a)] where
  toStructOrUnion loc a = (loc, PEUserStructOrUnion $ M.map (toExpr loc) $ M.fromList a)

instance ToExpr pe a => ToStructOrUnion pe [(String, a)] where
  toStructOrUnion loc a = (loc, PEUserStructOrUnion $ M.fromList $ map (pack *** toExpr loc) a)
