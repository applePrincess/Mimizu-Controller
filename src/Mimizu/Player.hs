{-# LANGUAGE RankNTypes #-}
{-|
Module      : Mimizu.Player
Copyright   : (c) Apple Princess 2018
License     : MIT
Maintainer  : Apple Princess
Stability   : experimental
Portability : portable
-}
module Mimizu.Player where

import Data.Bits (shiftR, (.&.))
import Data.Int (Int32)
import Data.Word (Word8, Word16, Word32)

import Mimizu.Util

-- | The type of the length
type Length = Word32

-- | The representation of player
data Player = Player { skin       :: [Color] -- ^ Skin information, a sequence of color
                     , name       :: String  -- ^ Name to be displayed
                     , excreta    :: Word32  -- ^ Counter to be shorten
                     , act        :: Word16  -- ^ Recent action
                     , playerInfo :: [Word8] -- ^ Joints and other info
                     }

-- | A constructor-like function, compatibility for JS original source
createPlayer :: [Word8] -> Player
createPlayer arr = if length arr >= 6
                   then Player [toEnum 15] "" 0 0 arr
                   else error $ "Invalid length of array given: " ++ show arr

-- | The length and an air value of the player specified
getVA :: Player -> Word32
getVA = conv8To32 . take 4 . playerInfo

-- | The most recent action taken by the player specified
getAct :: Player -> Word16
--getAct = conv8To16 . take 2 . drop 4 . playerInfo
getAct = act

-- | The current speed and `maximum speed` for the player specified
getSBM :: Player -> Word16
getSBM = conv8To16 . take 2 . drop 4 . playerInfo

-- | The rough x-location, this function is only available when
--   the distance between you and the player specified is enough
getUSX :: Player -> Word16
getUSX = conv8To16 . take 2 . drop 4 . playerInfo

-- | The rough y-location, this function is only available when
--   the distance between you and the player specified is enough
getUSY :: Player -> Word16
getUSY = conv8To16 . take 2 . drop 6 . playerInfo

-- | the counter when the player specified is overlapping/overlapped by other player.
--   when this counter is enough, `going straight` bug will occur.
getAir :: Player -> Word32
getAir = (`shiftR` 28) . getVA

-- | The angle being aimed by the player specified
getAn :: Player -> Word16
getAn = (.&. 0xfff) . getAct

-- | The flag whether dash button is been pressed by the player specified, true if pressed
getBtn :: Player -> Bool
getBtn = (/= 0) . (.&. 0x800) . getAct

-- | Number of joints of the player specified
getJN :: Player -> Int32
getJN = (\x -> (toEnum x::Int32) - 14 `div` 8) . length . playerInfo

-- | The `gear` the player currently in
getSH :: Player -> Float
getSH = max 1 . (fromIntegral :: Int -> Float) . (\x -> floor ((fromIntegral x::Float) / 500.0 - 2)) . getSpd

-- | Current speed of the player specified
getSpd :: Player -> Word16
getSpd = (.&. 0x1fff) . getSBM

-- | The actual length the specified player has
getVol :: Player -> Word32
getVol = (.&. 0xfffffff) . getVA
--     = `shiftR` 8 . getVA

-- | The thickness of the player.
getSR :: Player -> Float
getSR = sizeR . getVol

-- | Minimum turn radius
getTR :: Player -> Float
getTR pl = 350 / spd / max 1 (sh - 1)
  where spd = fromIntegral $ getSpd pl :: Float
        sh  = getSH pl

-- | The x location of head of the player specified.
--   if it is too far, return rough value by Left
--   otherwise, return precise value by Right
getX0 :: Player -> Either Word16 Float
getX0 pl = if getJN pl < 0 then Left (getUSX pl) else Right (getX 0 pl)

-- | The y location of head of the player specified.
--   if it is too far, return rough value by Left
--   otherwise, return precise value by Right
getY0 :: Player -> Either Word16 Float
getY0 pl = if getJN pl < 0 then Left (getUSY pl) else Right (getY 0 pl)

-- | Current Actual Head the player specified is going
getAngle :: Player -> Float
getAngle pl = atan2 (getY 0 pl - getY 1 pl) (getX 0 pl - getY 1 pl)

-- | The x location of idx-th joints of the player specified
getJointX :: Index -> Player -> Float
getJointX idx = conv8To32f . take 4 . drop (fromEnum idx*8+8) . playerInfo

-- | The y location of index-th joints of the player specified
getJointY :: Index -> Player -> Float
getJointY idx = conv8To32f . take 4 . drop (fromEnum idx*8+8+4) . playerInfo

-- | Size convert function from the length
sizeR :: Length -> Float
sizeR v = fromIntegral v ** 0.21875 * 7

-- | Compatibility function for getJointX and getJointY
getX, getY :: Index -> Player -> Float
getX = getJointX
getY = getJointY

modifyPlayerInfo :: [Word8] -> Maybe Player -> Maybe Player
modifyPlayerInfo d = maybe Nothing (\(Player s n e a _) ->  Just $ Player s n e a d)

modifyAction :: Word16 -> Maybe Player -> Maybe Player
modifyAction d = maybe Nothing (\(Player s n e _ p) ->  Just $ Player s n e d p)

modifyExcreta :: Word32 -> Maybe Player -> Maybe Player
modifyExcreta d = maybe Nothing (\(Player s n _ a p) ->  Just $ Player s n d a p)

modifyName :: String -> Maybe Player -> Maybe Player
modifyName d = maybe Nothing (\(Player s _ e a p) ->  Just $ Player s d e a p)

modifySkin :: [Color] -> Maybe Player -> Maybe Player
modifySkin d = maybe Nothing (\(Player _ n e a p) ->  Just $ Player d n e a p)
