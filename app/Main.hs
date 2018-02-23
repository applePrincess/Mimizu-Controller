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

chatCallback :: ChatCallback
chatCallback = undefined

-- mainLoop :: String -> ErrorHandler -> GameReceiveCallback -> ChatCallback -> IO ()

main :: IO ()
main = do
  putStrLn "What is your pid? "
  pid <- getLine
  ref <- newIORef 0 :: IO (IORef Int)
  mainLoop pid errorHandler (gameReceive ref) chatCallback
