module Mimizu.Util
  ( conv8To16
  , conv8To32
  , conv8To32f
  , conv16To8
  , conv32To8
  , convToFloat
  , Color(..)
  , colorToString ) where

import Data.Word (Word8, Word16, Word32)
import Data.Bits (shiftL, shiftR, (.&.))
import Unsafe.Coerce
conv8To16 :: [Word8] -> Word16
conv8To16 [x, y] = (unsafeCoerce x :: Word16) `shiftL` 8 + unsafeCoerce y :: Word16
conv8To16  v     = error $ "Unconvertible array found: " ++ show v

conv8To32 :: [Word8] -> Word32
conv8To32 [x, y, z, w] = (unsafeCoerce x :: Word32) `shiftL` 24 +
                         (unsafeCoerce y :: Word32) `shiftL` 16 +
                         (unsafeCoerce z :: Word32) `shiftL` 8 +
                         unsafeCoerce w :: Word32
conv8To32  v     = error $ "Unconvertible array found: " ++ show v

conv8To32f :: [Word8] -> Float
conv8To32f xs = if length xs == 4
                then convToFloat $ conv8To32  xs
                else error $ "Unconvertible array found: " ++ show xs

conv16To8 :: Word16 -> [Word8]
conv16To8 x = [ unsafeCoerce (x `shiftR` 8) :: Word8
              , unsafeCoerce (x .&. 0xff) :: Word8]

conv32To8 :: Word32 -> [Word8]
conv32To8 x = [ unsafeCoerce (x `shiftR` 24         ) :: Word8
              , unsafeCoerce (x `shiftR` 16 .&. 0xff) :: Word8
              , unsafeCoerce (x `shiftR`  8 .&. 0xff) :: Word8
              , unsafeCoerce (x .&. 0xff            ) :: Word8]

convToFloat :: Word32 -> Float
convToFloat = unsafeCoerce


data Color = NeonBlue | SummerSky | LimeGreen | Turquoise1
           | Turquoise2 | ElectricPurple | LavenderGray | GreenYellow
           | FringyFlower | RedOrange | RazzleDazzleRose1 | RazzleDazzleRose2
           | LightningYellow | BeautyBush | GoldenFizz | Whisper deriving Enum

colorToString :: Color -> String
colorToString c = case c of
  NeonBlue          -> "#3333ff"
  SummerSky         -> "#33bbff"
  LimeGreen         -> "#33ff33"
  Turquoise1        -> "#33ffbb"
  Turquoise2        -> "#33eeee"
  ElectricPurple    -> "#bb33ff"
  LavenderGray      -> "#bbbbee"
  GreenYellow       -> "#bbff33"
  FringyFlower      -> "#bbeebb"
  RedOrange         -> "#ff3333"
  RazzleDazzleRose1 -> "#ff33bb"
  RazzleDazzleRose2 -> "#ee33ee"
  LightningYellow   -> "#eeaa33"
  BeautyBush        -> "#eebbbb"
  GoldenFizz        -> "#eeee33"
  Whisper           -> "#eeeeee"
