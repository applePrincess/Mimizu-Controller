{-|
Module      : Mimizu.Food
Copyright   : (c) Apple Princess 2018
License     : MIT
Maintainer  : Apple Princess
Stability   : experimental
Portability : portable
-}

module Mimizu.Food where

import Data.Word (Word8)
import Data.Bits (shiftR, (.&.))

import Mimizu.Util

-- | The food info contained by one block.
newtype FoodBlock = FoodBlock {foodInfo :: [Word8]}

-- | color to be displayed
getColor :: Index -> FoodBlock -> Color
getColor idx = toEnum . fromEnum . (`shiftR` 4) . (!!(fromEnum idx*2)) . foodInfo

-- | size you can aquire when you eat, also size to be displayed
getSize :: Index -> FoodBlock -> Word8
getSize idx = (.&. 0x0f) . (!!(fromEnum idx*2)) . foodInfo

-- | x location of the food specified by index and food block
getFoodX :: Index -> FoodBlock -> Word8
getFoodX idx = (`shiftR` 4) . (!!(fromEnum idx*2+1)) . foodInfo

-- | y location of the food specified by index and food block
getFoodY :: Index -> FoodBlock -> Word8
getFoodY idx = (.&. 0x0f) . (!!(fromEnum idx*2+1)) . foodInfo


-- | compatibility funcction for getFoodX and getFoodY
getX, getY :: Index -> FoodBlock -> Word8
getX = getFoodX
getY = getFoodY
