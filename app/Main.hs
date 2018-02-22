
module Main (main) where

import Data.IORef
import Framework
-- import Mimizu

gameReceive :: IORef Int -> GameReceiveCallback
gameReceive ref pid players foods = do
  val <- readIORef ref
  modifyIORef ref (+1)
  putStrLn $ "ref: " ++ show val ++ " action: " ++
    show (val `mod` 4 == 0 :: DashFlag, (toEnum val `mod` 16 * 0x111) :: GameAngle)
  return (val `mod` 4 == 0 :: DashFlag, (toEnum val `mod` 16 * 0x111) :: GameAngle)

errorHandler :: ErrorHandler
errorHandler pid plyaers foods = putStrLn

main :: IO ()
main = do
  ref <- newIORef 0 :: IO (IORef Int)
  putStrLn "counter initialized"
  mainLoop errorHandler (gameReceive ref)
  putStrLn "main ends here."
