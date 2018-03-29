{-# LANGUAGE MultiWayIf #-}
module Main (main) where

import           Data.Bits  (shiftL, shiftR, (.&.))
import qualified Data.Map   as M
import           Data.Maybe (fromJust, fromMaybe, isJust, catMaybes)

import qualified Data.Text as T

import           Framework
import           Mimizu

{-
gameReceive :: IORef Int -> GameReceiveCallback
gameReceive ref pid players foods team = do
  val <- readIORef ref
  modifyIORef ref (+1)
  putStrLn $ "ref: " ++ show val ++ " action: " ++
    show ((toEnum val `mod` 16 * 0x111) :: GameAngle, val `mod` 4 == 0 :: DashFlag)
  return $ Just ((toEnum val `mod` 16 * 0x111) :: GameAngle, val `mod` 4 == 0 :: DashFlag)
-}

sampleGameReceive :: GameReceiveCallback
sampleGameReceive = do
  index <- playerID
  players' <- players
  foods' <- foods
  return $ actionDecision index players' foods'

actionDecision :: Index -> [Maybe Player] -> [FoodBlock] -> Maybe Action
actionDecision index playerList foodList| isBlueZone wx wy    = let [vx, vy] = [(0x8000 - wy),(0x8000 - wx)]
                                                                in Just (fromPositionf vx vy, False)
                                        | isJust ep && nn > 4 = let an'    = atan2 dy dx
                                                                    bb     = nn > 100
                                                                    posNeg = signum (diffAngle an' (angle pl))
                                                                    aa     = (fromIntegral (an pl) * pi / 0x800)
                                                                    aim    = floor $ (fromIntegral (an pl) - posNeg) *
                                                                             0x400 / ((if bb then 1 else 0) * 7 + 1)
                                                                in if abs (diffAngle aa (angle pl)) < 0.1
                                                                   then Just (aim .&. 0xfff, bb)
                                                                   else aimFood pl foodList
                                        | isJust ep              = Just (fromPositionf dy dx, False)
                                        | otherwise              = aimFood pl foodList
  where pl = fromJust $ playerList !! fromEnum index
        [wx, wy] = [playerWorldXf pl, playerWorldYf pl]
        ep = searchEnemy pl . catMaybes $ filter (/= (Just pl)) playerList
        [ex, ey] = [playerWorldXf $ fromJust ep, playerWorldYf $ fromJust ep]
        [dx, dy] = [max ex wx - min ex wx, max ey wy - min ey wy]
        nn = (dx *dx + dy * dy) / (sr pl * sr pl)

isBlueZone :: Double -> Double -> Bool
isBlueZone px py = px < 0x200 || px > 0xfe00 || py < 0x100 || py > 0xfe00

aimFood :: Player -> [FoodBlock] -> Maybe Action
aimFood pl foodList | isBlueZone wx wy = Nothing
                    | otherwise        = if foodCount f' == 0
                                         then Nothing
                                         else let (fx, fy)       = nearestFood pl [f']
                                                  (dx, dy)       = (fromIntegral fx - jointX 0 pl,
                                                                    fromIntegral fy - jointY 0 pl)
                                                  a              = fromRadian $ atan2 dy dx
                                              in  if dx * dx + dy * dy < sr pl * 4
                                                  then Nothing
                                                  else Just (a, False)
  where f' = foodList !! (toEnum . fromEnum $ by `shiftL` 8 + bx)
        [bx, by] = [playerWorldX pl `shiftR` 8, playerWorldY pl `shiftR` 8]
        [wx, wy] = [playerWorldXf pl, playerWorldYf pl]

searchEnemy :: Player -> [Player] -> Maybe Player
searchEnemy pl playerList = if fromIntegral (headDistance pl np) < defaultRange
                            then Just np
                            else Nothing
  where defaultRange = autoSearchRange * sr pl
        np           = nearestPlayerf pl playerList

autoSearchRange :: Double
autoSearchRange = 20000

errorHandler :: ErrorHandler
errorHandler = putStrLn

chatCallback :: ChatCallback
chatCallback = return $ T.pack ""

sendingFunction :: ChatCallback
sendingFunction = T.pack <$> getLine

main :: IO ()
main = do
  putStrLn "What is your pid? "
  pid <- getLine
--  ref <- newIORef 0 :: IO (IORef Int)
--  mainLoop pid errorHandler (gameReceive ref) chatCallback sendingFunction False
  mainLoop pid errorHandler sampleGameReceive chatCallback sendingFunction False
