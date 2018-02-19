module Mimizu.Food where

import Data.Word (Word8)
import Data.Bits (shiftR, (.&.))

import Mimizu.Util

newtype FoodBlock = FoodBlock {foodInfo :: [Word8]}


-- ^ color to be displayed
getColor :: Int -> FoodBlock -> Color
getColor idx = toEnum . fromEnum . (`shiftR` 4) . (!!(idx*2)) . foodInfo

-- ^ size you can aquire when you eat, also size to be displayed
getSize :: Int -> FoodBlock -> Word8
getSize idx = (.&. 0x0f) . (!!(idx*2)) . foodInfo

-- ^ x location of the food specified by index and food block
getFoodX :: Int -> FoodBlock -> Word8
getFoodX idx = (`shiftR` 4) . (!!(idx*2+1)) . foodInfo

-- ^ y location of the food specified by index and food block
getFoodY :: Int -> FoodBlock -> Word8
getFoodY idx = (.&. 0x0f) . (!!(idx*2+1)) . foodInfo


getX, getY :: Int -> FoodBlock -> Word8
-- ^ compatibility funcction
getX = getFoodX
getY = getFoodY
