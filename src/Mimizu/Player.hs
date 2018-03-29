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

import           Data.Bits   (shiftR, (.&.))
import           Data.Int    (Int32)
import           Data.List   (sortBy)
import           Data.Word   (Word8, Word16, Word32, Word64)

import           Mimizu.Util

-- | The type of the length
type Length = Word32

-- | Represnts an arrow key which being pressed, or not if it is None.
data ArrowKey = LeftArrow | RightArrow | None

-- | Must be in range 0 ~ 4095.
type GameAngle = Word16
-- | True if dash is on, False otherwise.
type DashFlag  = Bool

-- | The type represents an action.
type Action = (GameAngle, DashFlag)

-- | The type represents an action, including key state.
type ActionWithKey = (GameAngle, DashFlag, ArrowKey)

-- | The representation of player
data Player = Player { skin       :: [Color] -- ^ Skin information, a sequence of color
                     , name       :: String  -- ^ Name to be displayed
                     , excreta    :: Word32  -- ^ Counter to be shorten
                     , act        :: Word16  -- ^ Recent action
                     , playerInfo :: [Word8] -- ^ Joints and other info
                     , win        :: Word16  -- ^ Number of winings during A `Battle Royal`. 0 by default
                     , lose       :: Word16  -- ^ Number of losts during A `Battle Royal`. 0 by default
                     , highScore :: Word32   -- ^ The highest score recorded in a day (0:00:00-23:59:59 in JST)
                     } deriving (Eq, Show)

-- | A constructor-like function, compatibility for JS original source
createPlayer :: [Word8] -> Player
createPlayer arr = if length arr >= 6
                   then Player [toEnum 15] "" 0 0 arr 0 0 0
                   else error $ "Invalid length of array given: " ++ show arr

-- | The length and an air value of the player specified
va :: Player -> Word32
va pl = conv8To32 . take 4 $ playerInfo pl

-- | The current speed and `maximum speed` for the player specified
sbm :: Player -> Word16
sbm = conv8To16 . take 2 . drop 4 . playerInfo

-- | The rough x-location, this function is only available when
--   the distance between you and the player specified is enough
usx :: Player -> Word16
usx = conv8To16 . take 2 . drop 4 . playerInfo

-- | The rough y-location, this function is only available when
--   the distance between you and the player specified is enough
usy :: Player -> Word16
usy = conv8To16 . take 2 . drop 6 . playerInfo

-- | the counter when the player specified is overlapping/overlapped by other player.
--   when this counter is enough, `going straight` bug will occur.
air :: Player -> Word32
air = (`shiftR` 28) . va

-- | The angle being aimed by the player specified
an :: Player -> Word16
an = (.&. 0xfff) . act

-- | The flag whether dash button is been pressed by the player specified, true if pressed
btn :: Player -> Bool
btn = (/= 0) . (.&. 0x800) . act

-- | The flag whether dash button is been pressed by the player specified, true if pressed
key :: Player -> ArrowKey
key pl | act pl .&. 0x2000 /= 0 = LeftArrow
       | act pl .&. 0x4000 /= 0 = RightArrow
       | otherwise              = None


-- | Number of joints of the player specified
jn :: Player -> Int32
jn = (\len -> ((toEnum len::Int32) - 14) `div` 8) . length . playerInfo

-- | The `gear` the player currently in
sh :: Player -> Double
sh = max 1 . (fromIntegral :: Int -> Double) . (\s -> floor ((fromIntegral s::Double) / 500.0 - 2)) . spd

-- | Current speed of the player specified
spd :: Player -> Word16
spd = (.&. 0x1fff) . sbm

-- | The actual length the specified player has
vol :: Player -> Word32
vol = (.&. 0xfffffff) . va
--     = `shiftR` 8 . va

-- | The thickness of the player.
sr :: Player -> Double
sr = sizeR . vol

-- | Minimum turn radius
tr :: Player -> Double
tr pl = 350 / spd' / max 1 (sh pl - 1)
  where spd' = fromIntegral $ spd pl :: Double

-- | The x location of head of the player specified.
--   if it is too far, return rough value by Left
--   otherwise, return precise value by Right
x0 :: Player -> Either Word16 Double
x0 pl = if jn pl < 0 then Left (usx pl) else Right (x 0 pl)

-- | The y location of head of the player specified.
--   if it is too far, return rough value by Left
--   otherwise, return precise value by Right
y0 :: Player -> Either Word16 Double
y0 pl = if jn pl < 0 then Left (usy pl) else Right (y 0 pl)

-- | Current Actual Head the player specified is going
angle :: Player -> Double
angle pl = atan2 (y 0 pl - y 1 pl) (x 0 pl - y 1 pl)

-- | The x location of idx-th joints of the player specified
jointX :: Index -> Player -> Double
jointX idx = realToFrac . conv8To32f . take 4 . drop (fromEnum idx * 8 + 8) . playerInfo

-- | The y location of index-th joints of the player specified
jointY :: Index -> Player -> Double
jointY idx = realToFrac .conv8To32f . take 4 . drop (fromEnum idx*8+6+4) . playerInfo

-- | Size convert function from the length
sizeR :: Length -> Double
sizeR v = fromIntegral v ** 0.21875 * 7

-- | Compatibility function for 'joinxX' and 'jointY'
x, y :: Index -> Player -> Double
x = jointX
y = jointY

-- | The helper function for 'Framework.MutablePlayerList', it will modify the player info.
modifyPlayerInfo :: [Word8] -> Maybe Player -> Maybe Player
modifyPlayerInfo d = maybe Nothing (\pl ->  Just $ pl { playerInfo = d })

-- | The helper function for 'Framework.MutablePlayerList', it will modify the action.
modifyAction :: Word16 -> Maybe Player -> Maybe Player
modifyAction d = maybe Nothing (\pl ->  Just $ pl { act = d })

-- | The helper function for 'Framework.MutablePlayerList', it will modify the excreta
modifyExcreta :: Word32 -> Maybe Player -> Maybe Player
modifyExcreta d = maybe Nothing (\pl->  Just $ pl { excreta = d })

-- | The helper function for 'Framework.MutablePlayerList', it will modify the name
modifyName :: String -> Maybe Player -> Maybe Player
modifyName d = maybe Nothing (\pl ->  Just $ pl { name = d })

-- | The helper function for 'Framework.MutablePlayerList', it will modify the skin
modifySkin :: [Color] -> Maybe Player -> Maybe Player
modifySkin d = maybe Nothing (\pl ->  Just $ pl { skin = d })

-- | The helper function for 'Framework.MutablePlayerList', it will modify the skin
modifyWin :: Word16 -> Maybe Player -> Maybe Player
modifyWin d = maybe Nothing (\pl ->  Just $ pl { win = d })

-- | The helper function for 'Framework.MutablePlayerList', it will modify the skin
modifyLose :: Word16 -> Maybe Player -> Maybe Player
modifyLose d = maybe Nothing (\pl ->  Just $ pl { lose = d })

-- | The helper function for 'Framework.MutablePlayerList', it will modify the skin
modifyHighScore :: Word64 -> Maybe Player -> Maybe Player
modifyHighScore d = maybe Nothing (\pl ->  Just $ pl { highScore = d })

-- | Returns X location in world coordinate of specified Player
playerWorldX, playerWorldY :: Player -> Word16
playerWorldX pl = case x0 pl of
                    Left v  -> v
                    Right v -> floor $ v + sr pl * cos (angle pl)

playerWorldY pl = case y0 pl of
              Left v  -> v
              Right v -> floor $ v + sr pl * sin (angle pl)

-- | Returns X location in world coordinate of specified Player. This function is more precise than 'worldX', 'worldY'
playerWorldXf, playerWorldYf :: Player -> Double
playerWorldXf pl = case x0 pl of
              Left v  -> fromIntegral v
              Right v -> v + sr pl * cos (angle pl)
playerWorldYf pl = case y0 pl of
              Left v  -> fromIntegral v
              Right v -> v + sr pl * sin (angle pl)

-- | Compatibility function for 'playerWorldX' and 'playerWorldY'
worldX, worldY :: Player -> Word16
worldX = playerWorldX
worldY = playerWorldY

-- | Compatibility function for 'playerWorldXf' and 'playerWorldYf'
worldXf, worldYf :: Player -> Double
worldXf = playerWorldXf
worldYf = playerWorldYf

-- | Returns the distance between players, the distance is measured in World coordinate.
headDistance :: Player -> Player -> Word64
headDistance p1 p2 = floor .  sqrt . fromIntegral $ (p1x - p2x) ^ 2 + (p1y - p2y) ^ 2
  where p1x = worldX p1
        p1y = worldY p1
        p2x = worldX p2
        p2y = worldY p2

-- | Returns the distance between players, the distance is measured in World coordinate. This function is more precise than 'headDistance'
headDistancef :: Player -> Player -> Double
headDistancef p1 p2 = sqrt $ (p1x - p2x) ^^ 2 + (p1y - p2y) ^^ 2
  where p1x = worldXf p1
        p1y = worldYf p1
        p2x = worldXf p2
        p2y = worldYf p2

-- | Returns LT if first player's volume is less than the others' GT when greater than, EQ otherwise.
compareByRanking :: Player -> Player -> Ordering
compareByRanking p1 p2 = compare vx vy
  where vx = vol p1
        vy = vol p2

-- | Comparing relative distance to player specified at the first argument.
compareByDistanceToPlayer :: Player -> Player -> Player -> Ordering
compareByDistanceToPlayer pl p1 p2 = compare dx dy
  where dx = headDistance pl p1
        dy = headDistance pl p2

-- | Same as 'compareByDistanceToPlayer', but uses 'headDistancef' instead.
compareByDistanceToPlayerf :: Player -> Player -> Player -> Ordering
compareByDistanceToPlayerf pl p1 p2 = compare dx dy
  where dx = headDistancef pl p1
        dy = headDistancef pl p2

-- | Sorting playrs given, fits to the ranking.
sortByRanking  :: [Player] -> [Player]
sortByRanking = sortBy (flip compareByRanking)

-- | Sorting players given, excluding player itself.
sortByDistanceToPlayer :: Player -> [Player] -> [Player]
sortByDistanceToPlayer pl players = sortBy (compareByDistanceToPlayer pl) players'
  where players' = filter (/= pl) players

-- | Same as 'sortByDistanceToPlayer', but uses 'compareByDistanceToPlayerf' instead.
sortByDistanceToPlayerf :: Player -> [Player] -> [Player]
sortByDistanceToPlayerf pl players = sortBy (compareByDistanceToPlayerf pl) players'
  where players' = filter (/= pl) players


-- | Returns the nearest player to the player specified at the first argument.
nearestPlayer :: Player -> [Player] -> Player
nearestPlayer pl = head . sortByDistanceToPlayer pl

-- | Same as 'nearestPlayer' but uses 'sortByDistanceToPlayer' instead.
nearestPlayerf :: Player -> [Player] -> Player
nearestPlayerf pl = head . sortByDistanceToPlayerf pl


-- | Returns the action taken by player specified, ignoring key pressed.
action :: Player -> Action
action pl = (an pl, btn pl)

-- | Returns the action taken by player specified, acknowledging key pressed.
actionWithKey :: Player -> ActionWithKey
actionWithKey pl = (an pl, btn pl, key pl)
