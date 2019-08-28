{-# LANGUAGE TemplateHaskell #-}

module Asm.Core.Data.MetaKey
  ( MetaKey
  , mkCpuMetaKeys
  , metaPool
  , metaPoolCode
  , metaPoolConst
  , metaPoolLocal
  , metaPoolVar
  , metaCheck
  , metaCheckData
  , metaCheckData8
  , metaCheckFill
  , metaCheckFill8
  , MetaKeyMap
  , mkmEmpty
  , mkmInsert
  , mkmDelete
  , mkmLookup
  , MetaKeySet
  , mksEmpty
  , mksInsert
  , mksDelete
  , mksMember
  , mksUnion
  , mksToList
  ) where

import           Asm.Core.Data.MetaKey.Internal

mkCompilerMetaKeys
  [ "metaPool"
  , "metaPoolCode"
  , "metaPoolConst"
  , "metaPoolLocal"
  , "metaPoolVar"
  , "metaCheck"
  , "metaCheckData"
  , "metaCheckData8"
  , "metaCheckFill"
  , "metaCheckFill8"
  ]
