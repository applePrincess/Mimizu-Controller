module Main (main) where

import Framework
import Mimizu

chatReceived :: IO ()
chatReceived = undefined

main :: IO ()
main = do
  putStrLn "What is your ID? "
  id <- getLine
  putStrLn "What is your Password? "
  password <- getLine
--  chatOnly id password chatReceived
  putStrLn $ '\t':(id ++ "\t" ++ password)
  return ()
