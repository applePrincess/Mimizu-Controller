{-# LANGUAGE RankNTypes #-}
module Mimizu.Player where

import Data.Bits (shiftR, (.&.))
import Data.Int (Int32)
import Data.Word (Word8, Word16, Word32)

import Mimizu.Util

data Player = Player { skin :: [Color] -- ^ Skin information, a sequence of color
                     , name :: String  -- ^ Name to be displayed
                     , excreta :: Int  -- ^ Counter to be shorten
                     , act :: Word16   -- ^ Recent action
                     , playerInfo :: [Word8]} -- ^ Joints and other info

createPlayer :: [Word8] -> Player
createPlayer arr = if length arr >= 6
                   then Player [toEnum 15] "" 0 0 arr
                   else error $ "Invalid length of array given: " ++ show arr


-- ^ returns a length and an air value
getVA :: Player -> Word32
getVA = conv8To32 . take 4 . playerInfo

-- ^ returns the most recent action taken
getAct :: Player -> Word16
--getAct = conv8To16 . take 2 . drop 4 . playerInfo
getAct = act

-- ^ returns current speed and maximum speed
getSBM :: Player -> Word16
getSBM = conv8To16 . take 2 . drop 4 . playerInfo


-- ^ rough x location, this is available when
--   the distance between you and the player specified is far enough
getUSX :: Player -> Word16
getUSX = conv8To16 . take 2 . drop 4 . playerInfo

-- ^ rough y location, this is available when
--   the distance between you and the player specified is far enough
getUSY :: Player -> Word16
getUSY = conv8To16 . take 2 . drop 6 . playerInfo


-- ^ the counter when the player specified is overlapping/overlapped by other player.
--   when this counter is enough, `going straight` bug will be caught.
getAir :: Player -> Word32
getAir = (`shiftR` 28) . getVA

-- ^ the angle aimed the specified player want turn into
getAn :: Player -> Word16
getAn = (.&. 0xfff) . getAct

-- TODO : need to know what the blow is.
-- ^ the flag whether button is been pressed by the player specified
getBtn :: Player -> Bool
getBtn = (/= 0) . (.&. 0x800) . getAct

-- ^ number of joints of the player specified
getJN :: Player -> Int32
getJN = (\x -> (toEnum x::Int32) - 14 `div` 8) . length . playerInfo


-- ^ the `gear` the player currently in
getSH :: Player -> Float
getSH = max 1 . (fromIntegral :: Int -> Float) . (\x -> floor ((fromIntegral x::Float) / 500.0 - 2)) . getSpd

-- ^ current speed of the player specified
getSpd :: Player -> Word16
getSpd = (.&. 0x1fff) . getSBM

-- ^ the actual length 
getVol :: Player -> Word32
getVol = (.&. 0xfffffff) . getVA
--     = `shiftR` 8 . getVA
-- ^ the radius of minimum turning.
getSR :: Player -> Float
getSR = sizeR . getVol


getTR :: Player -> Float
getTR pl = 350 / spd / max 1 (sh - 1)
  where spd = fromIntegral $ getSpd pl :: Float
        sh  = getSH pl

-- ^ the x location of head of the player specified.
--   if it is too far, return rough value by Left
--   otherwise, return precise value by Right
getX0 :: Player -> Either Word16 Float
getX0 pl = if getJN pl < 0 then Left (getUSX pl) else Right (getX 0 pl)

-- ^ the y location of head of the player specified.
--   if it is too far, return rough value by Left
--   otherwise, return precise value by Right
getY0 :: Player -> Either Word16 Float
getY0 pl = if getJN pl < 0 then Left (getUSY pl) else Right (getY 0 pl)

-- ^ Current Actual Head the player specified is going
getAngle :: Player -> Float
getAngle pl = atan2 (getY 0 pl - getY 1 pl) (getX 0 pl - getY 1 pl)


-- ^ the x location of idx-th joints of the player specified
getJointX :: Word32 -> Player -> Float
getJointX idx = conv8To32f . take 4 . drop (fromEnum idx*8+8) . playerInfo

-- ^ the y location of idx-th joints of the player specified
getJointY :: Word32 -> Player -> Float
getJointY idx = conv8To32f . take 4 . drop (fromEnum idx*8+8+4) . playerInfo

-- ^ size convert function
sizeR :: Word32 -> Float
sizeR v = fromIntegral v ** 0.21875 * 7


getX, getY :: Word32 -> Player -> Float
-- ^ compatibility
getX = getJointX

-- ^ compatibility
getY = getJointY
