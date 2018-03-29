{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

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
--  , makeTriplets -- since this function is no longer work! due to format changing
  , PlayerExternalInfo
  , makeSextuplets
  , makeColors
  , diffAngle ) where

import           Data.Array.ST (newArray, readArray, MArray, STUArray)
import           Data.Array.Unsafe (castSTUArray)
import           Data.Bits     (shiftL, shiftR, (.&.))
import           Data.Fixed    (mod')
import           Data.Function ((&))
import           Data.List     (isInfixOf)
import           Data.Word     (Word16, Word32, Word8, Word64)
import           Control.Monad.ST (runST, ST)
import           Numeric       (readHex)

import           Data.Text     (Text, intercalate, pack, splitOn, unpack)

-- | Convert from raw websocket data to Uint16
conv8To16 :: [Word8] -> Word16
conv8To16 [x, y] = (conv x :: Word16) + (conv y :: Word16) `shiftL` 8
conv8To16  v     = error $ "Unconvertible array found: " ++ show v

-- | Convert from raw websocket data to list of Uint16.
--   Note: the length of list must be multiple of 2.
conv8To16s :: [Word8] -> [Word16]
conv8To16s xs = conv8To16 (take 2 xs) : conv8To16s (drop 2 xs)

-- | Convert from raw websocket data to Uint32
conv8To32 :: [Word8] -> Word32
conv8To32 [x, y, z, w] = (conv x :: Word32)             +
                         (conv y :: Word32) `shiftL` 8  +
                         (conv z :: Word32) `shiftL` 16 +
                         (conv w :: Word32) `shiftL` 24
conv8To32  v     = error $ "Unconvertible array found: " ++ show v

-- | Convert from raw websocket data to Float
conv8To32f :: [Word8] -> Float
conv8To32f xs = if length xs == 4
                then convToFloat $ conv8To32  xs
                else error $ "Unconvertible array found: " ++ show xs

-- | Convert from Uint16 to sendable data
conv16To8 :: Word16 -> [Word8]
conv16To8 x = [ conv (x `shiftR` 8) :: Word8
              , conv (x .&. 0xff) :: Word8]

-- | Convert from Uint32 to sendable data
conv32To8 :: Word32 -> [Word8]
conv32To8 x = [ conv (x             .&. 0xff) :: Word8
              , conv (x `shiftR`  8 .&. 0xff) :: Word8
              , conv (x `shiftR` 16 .&. 0xff) :: Word8
              , conv (x `shiftR` 24         ) :: Word8]

{-# INLINE conv #-}
conv :: (Enum a, Enum b) => a -> b
conv = toEnum . fromEnum

-- | Convert from Uint32 to Float
convToFloat :: Word32 -> Float
convToFloat x = runST (cast x)

{-# INLINE cast #-}
cast :: (MArray (STUArray s) a (ST s), MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0

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
           deriving (Enum, Eq, Show)

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
                 then conv num
                 else error $ "Out of range value found: " ++ show num

-- | Convert to Index type
integralToIndex :: (Enum a, Integral a) => a -> Index
integralToIndex = conv


type PlayerExternalInfo = (Index, String, [Color], Word16, Word16, Word32)

-- | Convert Text to list of Triplets
makeTriplets :: Text -> [(Index, String, [Color])]
makeTriplets txt = (read $ unpack x, unpack y, makeColors $ unpack z): case xs of
                                                               [] -> []
                                                               _  -> makeTriplets (intercalate (pack "\t") xs)
  where (x:y:z:xs) = splitOn (pack "\t") txt

-- | Convert Text to list of Quintuplets
makeSextuplets :: Text -> [PlayerExternalInfo]
makeSextuplets (lines . unpack -> ls) = makeSextuplet' ls
  where makeSextuplet' :: [String] -> [PlayerExternalInfo]
        makeSextuplet' (x:xs) = let [idx, name, col, w, l, hs] = if length ys /= 6 then head ys:"":tail ys else ys
                                    ys                         = words x
                                 in (read idx, name, makeColors col, read w, read l, read hs):
                                    case xs of
                                      [] -> []
                                      _  -> makeSextuplet' xs
        makeSextuplet' []    = []

-- | Convert framework color enum from string
makeColors :: String -> [Color]
makeColors = map (\c ->  toEnum $ head (readHex [c]) & fst)

-- | Caluculate difference between two angles.
diffAngle :: Double -> Double -> Double
diffAngle x y = if abs d1 > abs d2 then d2 else d1 -- if v > pi then v - 2*pi else v
  where x' = mod' x (2*pi) -- [0, 2*pi)
        y' = mod' y (2*pi) -- [0, 2*pi)
--        v = if x' > y' then x' - y' else y' - x'
        d1 = x' - y'
        d2 = y' - x'
