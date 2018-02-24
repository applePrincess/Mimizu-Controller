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
import           Data.Word   (Word8)

import           Mimizu.Util

-- | The food info contained by one block.
newtype FoodBlock = FoodBlock {foodInfo :: [Word8]} deriving (Eq, Show)

type Food = (Word8, Word8) -- ^ Tuple of x and y coordinates for specific food in the food block.
-- | Color to be displayed
getColor :: Index -> FoodBlock -> Color
getColor idx = toEnum . fromEnum . (`shiftR` 4) . (!!(fromEnum idx*2)) . foodInfo

-- | Size you can aquire when you eat, also size to be displayed
getSize :: Index -> FoodBlock -> Word8
getSize idx = (.&. 0x0f) . (!!(fromEnum idx*2)) . foodInfo

-- | X location of the food specified by index and food block
getFoodX :: Index -> FoodBlock -> Word8
getFoodX idx = (`shiftR` 4) . (!!(fromEnum idx*2+1)) . foodInfo

-- | Y location of the food specified by index and food block
getFoodY :: Index -> FoodBlock -> Word8
getFoodY idx = (.&. 0x0f) . (!!(fromEnum idx*2+1)) . foodInfo


-- | Compatibility funcction for getFoodX and getFoodY
getX, getY :: Index -> FoodBlock -> Word8
getX = getFoodX
getY = getFoodY

-- | Convert food block to list of foods
convertCoordinateInfo :: FoodBlock -> [Food]
convertCoordinateInfo = map (\x -> (x `shiftR` 4 .&. 0x0f, x .&. 0x0f)) . foodInfo
