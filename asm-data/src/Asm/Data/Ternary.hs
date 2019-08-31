module Asm.Data.Ternary
  ( Ternary
  , (*|)
  , (*&)
  , unknown
  , (*-)
  , (*+>)
  , (*<+)
  , (*<+>)
  , tMask
  , tUMask
  , tValue
  , tValueAndMask
  , tValueAndUMask
  , tValueFill
  , tValueFillAndMask
  , tValueFillAndUMask
  ) where

import           Data.Bits                 (Bits (..))

import           Asm.Data.Ternary.Internal

-- Ternary Invariant: All bits of the mask which are set (2nd argument) MUST be also set in the value (1st argument)
-- All functions here keep the invariant, see :* for the current constructor

-- the constructor (maintaining the invariant)
(*|) :: Bits b => b -> b -> Ternary b
{-# INLINE (*|) #-}
v *| m = (v .|. m) :*|: m

-- an alternative constructor (invert the mask)
(*&) :: Bits b => b -> b -> Ternary b
{-# INLINE (*&) #-}
v *& m = (v .|. comM) :*|: comM
  where
    comM = complement m

-- constructor for all bits unknown
unknown :: Bits b => Ternary b
{-# INLINE unknown #-}
unknown = allBits :*|: allBits
  where
    allBits = complement zeroBits

-- for all bits clear use `zeroBits`, for all bit set use `complement zeroBits`

-- remove data (add unknown) to the value  (v *| m <=> (v *| 0) *- m)
(*-) :: Bits b => Ternary b -> b -> Ternary b
{-# INLINE (*-) #-}
(v :*|: m) *- m2 = (v .|. m2) :*|: (m .|. m2)

-- add data (overwriting the old) to the value (right overwrites left)
(*+>) :: Bits b => Ternary b -> Ternary b -> Ternary b
{-# INLINE (*+>) #-}
(v1 :*|: m1) *+> (v2 :*|: m2) = (v1 .&. m2 .|. v2 .&. complement m2) :*|: (m1 .&. m2)

-- add data (overwriting the old) to the value (left overwrites right)
(*<+) :: Bits b => Ternary b -> Ternary b -> Ternary b
{-# INLINE (*<+) #-}
(v1 :*|: m1) *<+ (v2 :*|: m2) = (v2 .&. m1 .|. v1 .&. complement m1) :*|: (m1 .&. m2)

-- combine data
(*<+>) :: Bits b => Ternary b -> Ternary b -> Maybe (Ternary b)
{-# INLINE (*<+>) #-}
(v1 :*|: m1) *<+> (v2 :*|: m2) =
  if keep1 == keep2
    then Just $ ((m2 .&. v1) .|. (m1 .&. v2) .|. keep1) :*|: (m1 .&. m2)
    else Nothing
  where
    keepM = complement (m1 .|. m2)
    keep1 = v1 .&. keepM
    keep2 = v2 .&. keepM

-- get the mask (1 = Unknown, 0 = True/False depending on value)
tMask :: Bits b => Ternary b -> b
{-# INLINE tMask #-}
tMask (_ :*|: m) = complement m

-- get the mask (0 = Unknown, 1 = True/False depending on value)
tUMask :: Bits b => Ternary b -> b
{-# INLINE tUMask #-}
tUMask (_ :*|: m) = m

-- get the value, note: all bits which are set in the mask are undefined in the value (currently they're set but that may change)
tValue :: Bits b => Ternary b -> b
{-# INLINE tValue #-}
tValue (v :*|: _) = v

-- get the value, note: all bits which are set in the mask are undefined in the value (currently they're set but that may change)
tValueAndMask :: Bits b => Ternary b -> (b, b)
{-# INLINE tValueAndMask #-}
tValueAndMask (v :*|: m) = (v, complement m)

-- get the value, note: all bits which are set in the mask are undefined in the value (currently they're set but that may change)
tValueAndUMask :: Bits b => Ternary b -> (b, b)
{-# INLINE tValueAndUMask #-}
tValueAndUMask (v :*|: m) = (v, m)

-- get the value, fill all bits which are set to unknown with an specified filler
tValueFill :: Bits b => b -> Ternary b -> b
{-# INLINE tValueFill #-}
tValueFill f (v :*|: m) = (v .&. complement m) .|. (f .&. m)

-- get the value, fill all bits which are set to unknown with an specified filler
tValueFillAndMask :: Bits b => b -> Ternary b -> (b, b)
{-# INLINE tValueFillAndMask #-}
tValueFillAndMask f (v :*|: m) = ((v .&. complement m) .|. (f .&. m), complement m)

-- get the value, fill all bits which are set to unknown with an specified filler
tValueFillAndUMask :: Bits b => b -> Ternary b -> (b, b)
{-# INLINE tValueFillAndUMask #-}
tValueFillAndUMask f (v :*|: m) = ((v .&. complement m) .|. (f .&. m), m)
