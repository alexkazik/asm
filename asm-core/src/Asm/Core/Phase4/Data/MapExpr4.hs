{-# LANGUAGE NoImplicitPrelude #-}

module Asm.Core.Phase4.Data.MapExpr4
  ( mapExpr
  , mapExprInStmt
  , mapExprInStmtBlock
  ) where

import           Asm.Core.Prelude
import qualified Control.Arrow

import           Asm.Core.Data.Cpu
import           Asm.Core.Phase4.Data.Expr4
import           Asm.Core.Phase4.Data.Stmt4

mapExpr :: Cpu c => (Expr4 c -> Expr4 c) -> Expr4 c -> Expr4 c
mapExpr f (E4CpuExpr l a)               = E4CpuExpr l (cpuMapExprInExpr f a)
mapExpr f (E4ByteVal l v)               = E4ByteVal l (map f v)
mapExpr f (E4RangedInt loc l h r e)     = E4RangedInt loc l h r (f e)
mapExpr f (E4UserArray loc v e)         = E4UserArray loc (map f v) (map f e)
mapExpr f (E4UserArrayBVS l v e)        = E4UserArrayBVS l v (map f e)
mapExpr f (E4DerefArray l a b)          = E4DerefArray l (f a) (f b)
mapExpr f (E4DefineArray l a b)         = E4DefineArray l (f a) (map f b)
mapExpr f (E4TypeStruct l a)            = E4TypeStruct l (map (Control.Arrow.second f) a)
mapExpr f (E4TypeUnion l a)             = E4TypeUnion l (map (Control.Arrow.second f) a)
mapExpr f (E4UserStructOrUnion loc v e) = E4UserStructOrUnion loc (map f v) (map f e)
mapExpr f (E4DerefStruct l a b)         = E4DerefStruct l (f a) b
mapExpr f (E4Function l a b)            = E4Function l a (map f b)
mapExpr _ x                             = x

mapExprInStmt :: Cpu c => (Expr4 c -> Expr4 c) -> Stmt4 c -> Stmt4 c
mapExprInStmt f (S4IfBlock l a)       = S4IfBlock l (map (\(b,c,d) -> (b, mapExpr f c, mapExprInStmtBlock f d)) a)
mapExprInStmt f (S4CpuStmt l a)       = S4CpuStmt l (cpuMapExprInStmt f a)
mapExprInStmt f (S4For l a b c d e g) = S4For l a (f b) c (f d) (f e) (mapExprInStmtBlock f g)
mapExprInStmt _ x@S4LabelDefinition{} = x

mapExprInStmtBlock :: Cpu c => (Expr4 c -> Expr4 c) -> Stmt4Block c -> Stmt4Block c
mapExprInStmtBlock f = map (mapExprInStmt f)
