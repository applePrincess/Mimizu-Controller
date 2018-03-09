{-|
Module      : Mimizu.Food
Copyright   : (c) Apple Princess 2018
License     : MIT
Maintainer  : Apple Princess
Stability   : experimental
Portability : portable
-}

module Mimizu.Food where

import           Data.Bits   (shiftR, (.&.))
import           Data.Word   (Word8, Word16)

import           Mimizu.Util

-- | The food info contained by one block.
newtype FoodBlock = FoodBlock {foodInfo :: [Word8]} deriving (Eq, Show)

-- | Tuple of x and y coordinates for specific food in the food block.
type Food      = (Word8, Word8)
-- | Tuple of x and y coordinates for specific food in the world coordinate.
type WorldFood = (Word16, Word16)

-- | Color to be displayed.
color :: Index -> FoodBlock -> Color
color idx = toEnum . fromEnum . (`shiftR` 4) . (!!(fromEnum idx*2)) . foodInfo

-- | Size you can aquire when you eat, also size to be displayed.
size :: Index -> FoodBlock -> Word8
size index = (.&. 0x0f) . (!!(fromEnum index*2)) . foodInfo

-- | X location of the food specified by index and food block.
foodX :: Index -> FoodBlock -> Word8
foodX index = (`shiftR` 4) . (!!(fromEnum index*2+1)) . foodInfo

-- | Y location of the food specified by index and food block.
foodY :: Index -> FoodBlock -> Word8
foodY index = (.&. 0x0f) . (!!(fromEnum index*2+1)) . foodInfo

-- | Compatibility funcction for 'foodX' and 'foodY'
x, y :: Index -> FoodBlock -> Word8
x = foodX
y = foodY


-- | The number of foods contained by a food block.
foodCount :: FoodBlock -> Index
foodCount = toEnum . length . foodInfo

-- | Convert a food to world coordinate specified in first index-th block, second index-th food in food block.
foodWorldX, foodWorldY :: Index -> Index -> FoodBlock -> Word16
foodWorldX bIndex index foodBlock = toEnum $ fromEnum (foodX index foodBlock) * bx * 0x100
  where bx = toEnum . fromEnum $ bIndex .&. 0xff
foodWorldY bIndex index foodBlock = toEnum $ fromEnum (foodY index foodBlock) * by * 0x100
  where by = toEnum . fromEnum $ bIndex `shiftR` 8

-- | Compatibility funcction for 'foodWorldX' and 'foodWorldY'.
worldX, worldY  :: Index -> Index -> FoodBlock -> Word16
worldX = foodWorldX
worldY = foodWorldY

-- | Convert food block to list of 'Food's.
meshPositions :: FoodBlock -> [Food]
meshPositions = map (\food -> (food `shiftR` 8 .&. 0x0f, food .&. 0xff)) . foodInfo

-- | Convert food block to list of 'WorldFood's.
worldPositions :: Index -> FoodBlock -> [WorldFood]
worldPositions bIdx foods = map (\idx -> (worldX bIdx idx foods, worldY bIdx idx foods)) [0.. foodCount foods - 1]
