{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Asm.Core.Phases34.Data.CompilerState34
  ( CSM34(..)
  , getPoolStateC
  , getPoolDefinitionC
  , getPositionC
  , getPositionPoolC
  , module Asm.Core.Phases.Data.CompilerState1234
  ) where

import           Asm.Core.Prelude
import qualified Data.Map.Strict                        as M

import           Asm.Core.Control.CompilerError
import           Asm.Core.Data.FunctionKey
import           Asm.Core.Data.Reference
import           Asm.Core.Phases.Data.CompilerState1234
import           Asm.Core.Phases.Data.PoolDefinition
import           Asm.Core.Phases34.Data.Function
import           Asm.Core.Phases34.Data.PoolState
import           Asm.Data.InfInt64

class CompilerState1234 m => CSM34 m where
  -- function
  type CSM34Cpu m
  lookupFunctionC :: FunctionKey -> m [Function m (CSM34Cpu m)]
  -- pool
  toolPoolGetPoolStateC :: m (Map Reference PoolState)
  toolPoolGetPoolDefinitionC :: m (Map Reference PoolDefinition)
  -- position
  toolPositionGetC :: m (Map Reference (Maybe Reference, Either (InfInt64, InfInt64) Int64))
  setPositionC :: Reference -> (Maybe Reference, Either (InfInt64, InfInt64) Int64) -> m ()
  isPhase4C :: m Bool

-- pool

getPoolStateC :: CSM34 m => Reference -> m PoolState
getPoolStateC p = do
  csPool <- toolPoolGetPoolStateC
  $fromJustOrError [([], "pool " ++ show p ++ " not found")] $ M.lookup p csPool

getPoolDefinitionC :: CSM34 m => Reference -> m (Maybe PoolDefinition)
getPoolDefinitionC p = do
  csPoolDefinition <- toolPoolGetPoolDefinitionC
  case M.lookup p csPoolDefinition of
    Just d -> return $ Just d
    Nothing -> return $ M.foldr go' Nothing csPoolDefinition
      where
        go' d Nothing =
          if p `elem` pdPools d
            then Just d
            else Nothing
        go' _ r = r

-- position

getPositionC :: CSM34 m => Reference -> m (Either (InfInt64, InfInt64) Int64)
getPositionC n = do
  csPosition <- toolPositionGetC
  case M.lookup n csPosition of
    Just p  -> return (snd p)
    Nothing -> return (Left (minBound, maxBound))

getPositionPoolC :: CSM34 m => Reference -> m (Maybe Reference)
getPositionPoolC n = do
  csPosition <- toolPositionGetC
  fst <$> $fromJustOrError [([], "name " ++ show n ++ " not found")] (M.lookup n csPosition)
