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
  , foodDistance
  , compareByFoodDistance
  , sortByFoodDistance ) where

import           Data.List (sortBy)

import           Mimizu.Chat
import           Mimizu.Food   hiding (getX, getY)
import           Mimizu.Player hiding (getX, getY)
import           Mimizu.Util

nearestFood :: Player -> [FoodBlock] -> Food
nearestFood pl block = head . sortByFoodDistance pl $ convertCoordinateInfo f
  where bx  = floor $ getJointX 0 pl  / 0x100
        by  = floor $ getJointY 0 pl  / 0x100
        f   = block !!(by * 0x100 + bx)

foodDistance :: Player -> Food -> Float
foodDistance pl f = sqrt $ (fx - plx) ^^ 2 + (fy - ply) ^^ 2
  where fx = fromIntegral $ fst f * 0x10
        fy = fromIntegral $ snd f * 0x10
        plx = getJointX 0 pl
        ply = getJointY 0 pl

compareByFoodDistance :: Player -> Food -> Food -> Ordering
compareByFoodDistance pl f1 f2 = compare (foodDistance pl f1) (foodDistance pl f2)

sortByFoodDistance :: Player -> [Food] -> [Food]
sortByFoodDistance pl = sortBy (compareByFoodDistance pl)
