{-|
Module      : Mimizu.Util
Copyright   : (c) Apple Princess 2018
License     : MIT
Maintainer  : Apple Princess
Stability   : experimental
Portability : portable
-}

module Mimizu.Util
  ( conv8To16
  , conv8To32
  , conv8To32f
  , conv8To16s
  , conv16To8
  , conv32To8
  , convToFloat
  , Color(..)
  , colorToString
  , fromStringToColor
  , Index
  , intToWord8
  , integralToIndex
  , makeTriplets
  , makeColors ) where

import Data.Bits (shiftL, shiftR, (.&.))
import Data.Function ((&))
import Data.Word (Word8, Word16, Word32)
import  Numeric (readHex)

import Unsafe.Coerce

import Data.Text (pack, splitOn, Text, intercalate, unpack)

-- | Convert from raw websocket data to Uint16
conv8To16 :: [Word8] -> Word16
conv8To16 [x, y] = (unsafeCoerce x :: Word16) `shiftL` 8 + unsafeCoerce y :: Word16
conv8To16  v     = error $ "Unconvertible array found: " ++ show v

-- | Convert from raw websocket data to list of Uint16.
--   Note: the length of list must be multiple of 2.
conv8To16s :: [Word8] -> [Word16]
conv8To16s xs = conv8To16 (take 2 xs) : conv8To16s (drop 2 xs)

-- | Convert from raw websocket data to Uint32
conv8To32 :: [Word8] -> Word32
conv8To32 [x, y, z, w] = (unsafeCoerce x :: Word32) `shiftL` 24 +
                         (unsafeCoerce y :: Word32) `shiftL` 16 +
                         (unsafeCoerce z :: Word32) `shiftL` 8 +
                         unsafeCoerce w :: Word32
conv8To32  v     = error $ "Unconvertible array found: " ++ show v

-- | Convert from raw websocket data to Float
conv8To32f :: [Word8] -> Float
conv8To32f xs = if length xs == 4
                then convToFloat $ conv8To32  xs
                else error $ "Unconvertible array found: " ++ show xs

-- | Convert from Uint16 to sendable data
conv16To8 :: Word16 -> [Word8]
conv16To8 x = [ unsafeCoerce (x `shiftR` 8) :: Word8
              , unsafeCoerce (x .&. 0xff) :: Word8]

-- | Convert from Uint32 to sendable data
conv32To8 :: Word32 -> [Word8]
conv32To8 x = [ unsafeCoerce (x `shiftR` 24         ) :: Word8
              , unsafeCoerce (x `shiftR` 16 .&. 0xff) :: Word8
              , unsafeCoerce (x `shiftR`  8 .&. 0xff) :: Word8
              , unsafeCoerce (x .&. 0xff            ) :: Word8]

-- | Convert from Uint32 to Float
convToFloat :: Word32 -> Float
convToFloat = unsafeCoerce

-- | The representation of color palette.
data Color = NeonBlue          -- ^ The clolor of #3333ff
           | SummerSky         -- ^ The clolor of #33bbff
           | LimeGreen         -- ^ The clolor of #33ff33
           | Turquoise1        -- ^ The clolor of #33ffbb
           | Turquoise2        -- ^ The clolor of #33eeee
           | ElectricPurple    -- ^ The clolor of #bb33ff
           | LavenderGray      -- ^ The clolor of #bbbbee
           | GreenYellow       -- ^ The clolor of #bbff33
           | FringyFlower      -- ^ The clolor of #bbeebb
           | RedOrange         -- ^ The clolor of #ff3333
           | RazzleDazzleRose1 -- ^ The clolor of #ff33bb
           | RazzleDazzleRose2 -- ^ The clolor of #ee33ee
           | LightningYellow   -- ^ The clolor of #eeaa33
           | BeautyBush        -- ^ The clolor of #eebbbb
           | GoldenFizz        -- ^ The clolor of #eeee33
           | Whisper           -- ^ The clolor of #eeeeee
           deriving Enum

-- | Convert Color to CSS acceptable string
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

-- | Convert css color hex-triplet to Color
fromStringToColor :: String -> Color
fromStringToColor str = case str of
  "#3333ff" -> NeonBlue
  "#33bbff" -> SummerSky
  "#33ff33" -> LimeGreen
  "#33ffbb" -> Turquoise1
  "#33eeee" -> Turquoise2
  "#bb33ff" -> ElectricPurple
  "#bbbbee" -> LavenderGray
  "#bbff33" -> GreenYellow
  "#bbeebb" -> FringyFlower
  "#ff3333" -> RedOrange
  "#ff33bb" -> RazzleDazzleRose1
  "#ee33ee" -> RazzleDazzleRose2
  "#eeaa33" -> LightningYellow
  "#eebbbb" -> BeautyBush
  "#eeee33" -> GoldenFizz
  "#eeeeee" -> Whisper
  _         -> error $ "Unrecognized color string found: " ++ str

-- | Index type
type Index = Word32


-- | Convert Haskell Int to Word8
intToWord8 :: Int -> Word8
intToWord8 num = if num >= 0 && num < 256
                 then unsafeCoerce num
                 else error $ "Out of range value found: " ++ show num

-- | Convert to Index type
integralToIndex :: Integral a => a -> Index
integralToIndex = unsafeCoerce

-- | Convert Text to list of Triplets
makeTriplets :: Text -> [(Index, String, [Color])]
makeTriplets txt = (read $ unpack x, unpack y, makeColors $ unpack z):case xs of
                                                               [] -> []
                                                               _  -> makeTriplets (intercalate (pack "\t") xs)
  where (x:y:z:xs) = splitOn (pack "\t") txt

-- | Convert framework color enum from string
makeColors :: String -> [Color]
makeColors = map (\c ->  toEnum $ head (readHex [c]) & fst)
