module Asm.C64.Colors where

import           Asm.Cpu6502

black, white, red, cyan :: Int64
{-# INLINE black #-}
black = 0
{-# INLINE white #-}
white = 1
{-# INLINE red #-}
red = 2
{-# INLINE cyan #-}
cyan = 3

purple, green, blue, yellow :: Int64
{-# INLINE purple #-}
purple = 4
{-# INLINE green #-}
green = 5
{-# INLINE blue #-}
blue = 6
{-# INLINE yellow #-}
yellow = 7

orange, brown, lightRed, darkGray :: Int64
{-# INLINE orange #-}
orange = 8
{-# INLINE brown #-}
brown = 9
{-# INLINE lightRed #-}
lightRed = 10
{-# INLINE darkGray #-}
darkGray = 11

gray, lightGreen, lightBlue, lightGray :: Int64
{-# INLINE gray #-}
gray = 12
{-# INLINE lightGreen #-}
lightGreen = 13
{-# INLINE lightBlue #-}
lightBlue = 14
{-# INLINE lightGray #-}
lightGray = 15

darkGrey, grey, lightGrey :: Int64
{-# INLINE darkGrey #-}
darkGrey = darkGray
{-# INLINE grey #-}
grey = gray
{-# INLINE lightGrey #-}
lightGrey = lightGray

black4, white4, red4, cyan4 :: TInt64
{-# INLINE black4 #-}
black4 = 0 *& 0x0f
{-# INLINE white4 #-}
white4 = 1 *& 0x0f
{-# INLINE red4 #-}
red4 = 2 *& 0x0f
{-# INLINE cyan4 #-}
cyan4 = 3 *& 0x0f

purple4, green4, blue4, yellow4 :: TInt64
{-# INLINE purple4 #-}
purple4 = 4 *& 0x0f
{-# INLINE green4 #-}
green4 = 5 *& 0x0f
{-# INLINE blue4 #-}
blue4 = 6 *& 0x0f
{-# INLINE yellow4 #-}
yellow4 = 7 *& 0x0f

orange4, brown4, lightRed4, darkGray4 :: TInt64
{-# INLINE orange4 #-}
orange4 = 8 *& 0x0f
{-# INLINE brown4 #-}
brown4 = 9 *& 0x0f
{-# INLINE lightRed4 #-}
lightRed4 = 10 *& 0x0f
{-# INLINE darkGray4 #-}
darkGray4 = 11 *& 0x0f

gray4, lightGreen4, lightBlue4, lightGray4 :: TInt64
{-# INLINE gray4 #-}
gray4 = 12 *& 0x0f
{-# INLINE lightGreen4 #-}
lightGreen4 = 13 *& 0x0f
{-# INLINE lightBlue4 #-}
lightBlue4 = 14 *& 0x0f
{-# INLINE lightGray4 #-}
lightGray4 = 15 *& 0x0f

darkGrey4, grey4, lightGrey4 :: TInt64
{-# INLINE darkGrey4 #-}
darkGrey4 = darkGray4
{-# INLINE grey4 #-}
grey4 = gray4
{-# INLINE lightGrey4 #-}
lightGrey4 = lightGray4

black8, white8, red8, cyan8 :: Word8
{-# INLINE black8 #-}
black8 = 0
{-# INLINE white8 #-}
white8 = 1
{-# INLINE red8 #-}
red8 = 2
{-# INLINE cyan8 #-}
cyan8 = 3

purple8, green8, blue8, yellow8 :: Word8
{-# INLINE purple8 #-}
purple8 = 4
{-# INLINE green8 #-}
green8 = 5
{-# INLINE blue8 #-}
blue8 = 6
{-# INLINE yellow8 #-}
yellow8 = 7

orange8, brown8, lightRed8, darkGray8 :: Word8
{-# INLINE orange8 #-}
orange8 = 8
{-# INLINE brown8 #-}
brown8 = 9
{-# INLINE lightRed8 #-}
lightRed8 = 10
{-# INLINE darkGray8 #-}
darkGray8 = 11

gray8, lightGreen8, lightBlue8, lightGray8 :: Word8
{-# INLINE gray8 #-}
gray8 = 12
{-# INLINE lightGreen8 #-}
lightGreen8 = 13
{-# INLINE lightBlue8 #-}
lightBlue8 = 14
{-# INLINE lightGray8 #-}
lightGray8 = 15

darkGrey8, grey8, lightGrey8 :: Word8
{-# INLINE darkGrey8 #-}
darkGrey8 = darkGray8
{-# INLINE grey8 #-}
grey8 = gray8
{-# INLINE lightGrey8 #-}
lightGrey8 = lightGray8

-- transparent pixels

transparent :: Int64
{-# INLINE transparent #-}
transparent = 16

transparent8 :: Word8
{-# INLINE transparent8 #-}
transparent8 = 16
