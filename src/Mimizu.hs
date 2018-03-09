{-|
Module      : Mimizu
Copyright   : (c) Apple Princess 2018
License     : MIT
Maintainer  : Apple Princess
Stability   : experimental
Portability : portable
-}
module Mimizu
  ( module Mimizu.Player
  , module Mimizu.Util
  , module Mimizu.Food
  , module Mimizu.Chat
  , nearestFood
  , nearestWorldFood
  , worldFoodDistance
  , foodDistance
  , compareByWorldFoodDistance
  , compareByFoodDistance
  , sortByWorldFoodDistance
  , sortByFoodDistance
  , sortByWorldFoodDistanceFromFoodBlock
  , sortByFoodDistanceFromFoodBlock ) where

import           Data.Bits (shiftL, shiftR, (.&.))
import           Data.List (sortBy)
import           Data.Word (Word8)

import           Mimizu.Chat
import           Mimizu.Food   hiding (x, y, worldX, worldY)
import           Mimizu.Player hiding (x, y, worldX, worldY, worldXf, worldYf)
import           Mimizu.Util

-- | Returns the minimum distance to a food from the player, in world coordinate.
nearestWorldFood :: Player -> [FoodBlock] -> WorldFood
nearestWorldFood pl = head . sortByWorldFoodDistance pl . concat . zipWith worldPositions [1..]


-- | Returns the food in the block which the player is in, in mesh coordinate.
nearestFood :: Player -> [FoodBlock] -> Food
nearestFood pl food = head $ sortByFoodDistance pl f
  where bx = floor (jointX 0 pl) `shiftR` 8
        by = floor (jointY 0 pl) `shiftR` 8
        f = meshPositions $ food !! (by `shiftL` 8 + bx)

-- | Calculate relational distance to the player in world coordinate.
worldFoodDistance :: Player -> WorldFood -> Float
worldFoodDistance pl (fwx, fwy) = sqrt . fromIntegral $ dx ^ 2 + dy ^ 2
  where px = playerWorldX pl
        py = playerWorldY pl
        dx = max px fwx - min px fwx
        dy = max py fwy - min py fwy

-- | Calculate relational distance to the player in mesh coordinate.
foodDistance :: Player -> Food -> Float
foodDistance pl f = sqrt . fromIntegral $ dx ^ 2 + dy ^ 2
  where fx = fst f
        fy = snd f
        plx = floor (jointX 0 pl) .&. 0xf0 `shiftR` 4 :: Word8
        ply = floor (jointY 0 pl) .&. 0xf0 `shiftR` 4 :: Word8
        dx = max fx plx - min fx plx -- ensure always non-negative
        dy = max fy ply - min fy ply -- ditto
{-# WARNING foodDistance "This function does not return the correct value when the block tha food is contained by is different from the player is in." #-}

-- | Compareing the relational distance to the player specified using 'worldFoodDistance'
--   nearer comes first, further comes second.
compareByWorldFoodDistance :: Player -> WorldFood -> WorldFood -> Ordering
compareByWorldFoodDistance pl f1 f2 = compare (worldFoodDistance pl f1) (worldFoodDistance pl f2)

-- | Compareing the relational distance to the player specified using 'foodDistance'
--   nearer comes first, further comes second.
compareByFoodDistance :: Player -> Food -> Food -> Ordering
compareByFoodDistance pl f1 f2 = compare (foodDistance pl f1) (foodDistance pl f2)

-- | Sort foods by relational distance to the player, using 'compareByWorldFoodDistance'.
sortByWorldFoodDistance :: Player -> [WorldFood] -> [WorldFood]
sortByWorldFoodDistance pl = sortBy (compareByWorldFoodDistance pl)

-- | Sort foods by relational distance to the player, using 'foodDistance'.
sortByFoodDistance :: Player -> [Food] -> [Food]
sortByFoodDistance pl = sortBy (compareByFoodDistance pl)

-- | The same as 'sortByFoodDistance', but this function takes 'FoodBlock' insted of a list of 'Food'
sortByFoodDistanceFromFoodBlock :: Player -> FoodBlock -> [Food]
sortByFoodDistanceFromFoodBlock pl = sortByFoodDistance pl . meshPositions

-- | Similar to 'sortByFoodDistanceFromFoodBlock', but this function also takes a block index,
--   returns in world coordinate.
sortByWorldFoodDistanceFromFoodBlock :: Player -> Index -> FoodBlock -> [WorldFood]
sortByWorldFoodDistanceFromFoodBlock pl index = sortByWorldFoodDistance pl . worldPositions index
