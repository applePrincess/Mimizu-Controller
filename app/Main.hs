{-# LANGUAGE MultiWayIf #-}
module Main (main) where

import           Data.Bits           (shiftL, shiftR, (.&.))
import           Data.IORef
import qualified Data.Map      as M
import           Data.Maybe         (fromJust, isJust, catMaybes)

import qualified Data.Text    as T

import Debug.Trace

import Framework
import Mimizu

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
sampleGameReceive index players foods team = return $ actionDecision index players foods

actionDecision :: Index -> [(Index, Maybe Player)] -> [(Index, FoodBlock)] -> Maybe Action
actionDecision index players foods | isBlueZone wx wy    = trace ("wx, wy = " ++ show (wx, wy)) $
                                                           let vx = fromIntegral (0x8000 - fromEnum wy)
                                                               vy = fromIntegral (0x8000 - fromEnum wx)
                                                           in Just (fromPosition vx vy, False)
                                   | isJust ep && nn > 4 = let an'    = atan2 (fromIntegral dx) (fromIntegral dy)
                                                               bb     = nn > 100
                                                               bb'    = if bb then 1 else 0
                                                               posNeg = signum (diffAngle an' (angle pl))
                                                               aa     = (fromIntegral (an pl) * pi / 0x800)
                                                               aim    = floor $ (fromIntegral (an pl) - posNeg) *
                                                                        0x400 / (bb' * 7 + 1)
                                                           in if abs (diffAngle aa (angle pl)) < 0.1
                                                              then Just (aim .&. 0xfff, bb)
                                                              else aimFood pl foods
                                   | isJust ep              = Just (fromPosition (fromIntegral dy) (fromIntegral dx)
                                                                   , False)
                                   | otherwise              = aimFood pl foods
  where isBlueZone bx by = bx < 0x100 || bx > 0xff00 || by < 0x100 || by > 0xff00
        pl               = fromJust . fromJust $ lookup index players
        wx               = playerWorldX pl
        wy               = playerWorldY pl
        ep               = searchEnemy pl . catMaybes . M.elems . M.fromList $ filter (\(idx, _) -> idx /= index)
                                                                               players
        ex               = playerWorldX $ fromJust ep
        ey               = playerWorldY $ fromJust ep
        dx               = max ex wx - min ex wx
        dy               = max ey wy - min ey wy
        nn               = fromIntegral (dx *dx + dy * dy) / (sr pl * sr pl)

aimFood :: Player -> [(Index,FoodBlock)] -> Maybe Action
aimFood pl foods = trace ("aiming food" ++ show f' ++ "bx = " ++ show bx ++ "by = " ++ show by) $
                   case f' of
                       Nothing  -> Nothing
                       (Just f) -> if foodCount f == 0
                                   then Nothing
                                   else let (fx, fy)       = nearestFood pl [f]
                                            (dx, dy)       = (fromIntegral fx - jointX 0 pl,
                                                              fromIntegral fy - jointY 0 pl)
                                            a              = fromRadian $ atan2 dy dx
                                        in  if dx * dx + dy * dy < sr pl * 4
                                            then Nothing
                                            else Just (a, False)
  where f' = lookup (toEnum . fromEnum $ by `shiftL` 8 + bx) foods
        bx = playerWorldX pl `shiftR` 8
        by = playerWorldY pl `shiftR` 8

-- a lot of duplication... need to be fixed. by some day.
searchEnemy :: Player -> [Player] -> Maybe Player
searchEnemy pl players = if fromIntegral (headDistance pl np) < defaultRange
                         then Just np
                         else Nothing
  where defaultRange = autoSearchRange * sr pl
        np           = nearestPlayer pl players

autoSearchRange = 20000

errorHandler :: ErrorHandler
errorHandler pid plyaers foods = putStrLn

chatCallback :: ChatCallback
chatCallback chat ranking = return $ T.pack ""

sendingFunction :: ChatCallback
sendingFunction chat ranking = T.pack <$> getLine

main :: IO ()
main = do
  putStrLn "What is your pid? "
  pid <- getLine
--  ref <- newIORef 0 :: IO (IORef Int)
--  mainLoop pid errorHandler (gameReceive ref) chatCallback sendingFunction False
  mainLoop pid errorHandler sampleGameReceive chatCallback sendingFunction False
