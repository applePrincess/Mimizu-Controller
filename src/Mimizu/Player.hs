{-# LANGUAGE RankNTypes #-}
module Mimizu.Player where

import Data.Bits (shiftR, (.&.))
import Data.Int (Int32)
import Data.Word (Word8, Word16, Word32)

import Mimizu.Util

data Player = Player { skin :: [Color]
                     , name :: String
                     , excreta :: Int
                     , playerInfo :: [Word8]}


-- this function is unsafe when empty array is given.
createPlayer :: [Word8] -> Player
createPlayer arr = if length arr >= 8
                   then Player [toEnum 15] "" 0 arr
                   else error $ "Invalid length of array given: " ++ show arr

getVA :: Player -> Word32
getVA = conv8To32 . take 4 . playerInfo

getAct :: Player -> Word16
getAct = conv8To16 . take 2 . drop 4 . playerInfo

getSBM :: Player -> Word16
getSBM = conv8To16 . take 2 . drop 6 . playerInfo


getUSX :: Player -> Word16
getUSX = getAct -- different meaning but the same implementation
getUSY :: Player -> Word16
getUSY = getSBM -- ditto

getAir :: Player -> Word32
getAir = (`shiftR` 28) . getVA

getAn :: Player -> Word16
getAn = (.&. 0xfff) . getAct

getBtn :: Player -> Bool
getBtn = (/= 0) . (.&. 0x800) . getAct

getJN :: Player -> Int32
getJN = (\x -> (toEnum x::Int32) - 16 `div` 8) . length . playerInfo

getSH :: Player -> Float
getSH = max 1 . (fromIntegral :: Int -> Float) . (\x -> floor ((fromIntegral x::Float) / 500.0 - 2)) . getSpd

getSpd :: Player -> Word16
getSpd = (.&. 0x1fff) . getSBM

getVol :: Player -> Word32
getVol = (.&. 0xfffffff) . getVA
--     = `shiftR` 8 . getVA

getSR :: Player -> Float
getSR = sizeR . getVol

getTR :: Player -> Float
getTR pl = 350 / spd / max 1 (sh - 1)
  where spd = fromIntegral $ getSpd pl :: Float
        sh  = getSH pl

-- TODO: need to know what server side program does
getX0 :: Player -> Either Word16 Float
getX0 pl = if getJN pl < 0 then Left (getUSX pl) else Right (getX 0 pl)

getY0 :: Player -> Either Word16 Float
getY0 pl = if getJN pl < 0 then Left (getUSY pl) else Right (getY 0 pl)

getAngle :: Player -> Float
getAngle pl = atan2 (getY 0 pl - getY 1 pl) (getX 0 pl - getY 1 pl)

getJointX :: Word32 -> Player -> Float
getJointX idx = conv8To32f . take 4 . drop (fromEnum idx*8+8) . playerInfo

getJointY :: Word32 -> Player -> Float
getJointY idx = conv8To32f . take 4 . drop (fromEnum idx*8+8+4) . playerInfo

sizeR :: Word32 -> Float
sizeR v = fromIntegral v ** 0.21875 * 7

getX = getJointX
getY = getJointY
