module Mimizu.Food where

import Data.Word (Word8)
import Data.Bits (shiftR, (.&.))

import Mimizu.Util

newtype FoodBlock = FoodBlock {foodInfo :: [Word8]}

getColor :: Int -> FoodBlock -> Color
getColor idx = toEnum . fromEnum . (`shiftR` 4) . (!!(idx*2)) . foodInfo

getSize :: Int -> FoodBlock -> Word8
getSize idx = (.&. 0x0f) . (!!(idx*2)) . foodInfo


getFoodX :: Int -> FoodBlock -> Word8
getFoodX idx = (`shiftR` 4) . (!!(idx*2+1)) . foodInfo

getFoodY :: Int -> FoodBlock -> Word8
getFoodY idx = (.&. 0x0f) . (!!(idx*2+1)) . foodInfo

getX = getFoodX
getY = getFoodY
