{-# LANGUAGE MultiWayIf #-}
module Framework where

import Data.IORef
import Data.Word
import Network.Socket
-- import Network.WebSocket

import Mimizu

-- hostAddress :: (Word8, Word8, Word8, Word8)
-- hostAddress = tupleToHostAddress (160, 16, 82, 222)

toAddrString :: (Word8, Word8, Word8, Word8) -> String
toAddrString (x, y, z, w) = show x ++ ":" ++ show y ++ ":" ++ show z ++ ":" ++ show w

gamePort :: PortNumber
gamePort = 8888

chatPort :: PortNumber
chatPort = 8891

-- gameHostAddr :: SockAddr
-- gameHostAddr = SocketAddr Inet gamePort hostAddress

-- chatHostAddr :: SockAddr
-- chatHostAddr = SocketAddr Inet chatPort hostAddress

-- ^ Current list of players, may or may not be upto dated depending on the distance from you
--   Current list of food blocks, it will not be upto dated for the block where the block you are not in.
--   Action.
type GameReceiveCallback  = [(Index, Player)] -> [(Index, FoodBlock)] -> IO Word16
type MutablePlayerList = [(Index, IORef Player)]
type MutableFoodList = [(Index, IORef FoodBlock)]

-- pullIORefs :: [(Index, IORef a)] -> IO [(Index, a)]
-- pullIORefs xs = mapM (\(idx, ref) -> readIORef ref >>= \x -> (idx, x)) xs

{-
run :: GameReceiveCallback -> ClientApp ()
run cb conn = do
  players <- newIORef [] :: IO MutablePlayerList
  foods <- newIORef [] :: IO MutableFoodList
  putStrLn " Game socket: Connected"
  _ <- forkIO $ forever $ do
    msg <- receiveData conn
    parseGameData players foods msg
    players' <- readIORef players
    foods'   <- readIORef foods
    act      <- cb players foods
    sendData conn act
  forever (return ())


mainLoop :: GameReceiveCallback -> IO ()
mainLoop cb = withSocketsDo $ runClient (toAddrString hostAddress) gamePort (run cb)
-}

parsePlayer, parseAction, parseDeath  :: MutablePlayerList -> Index ->  [Word8] -> IO ()
parseAction = undefined
parseDeath = undefined
parsePlayer = undefined

parseFood :: MutableFoodList  -> Index -> [Word8] -> IO ()
parseFood = undefined

-- ^ A tag for parsing data
playerParseTag, actionParseTag, foodParseTag, deathParseTag, numberParseTag :: Word8

playerParseTag = intToWord8 $ fromEnum 'P'
actionParseTag = intToWord8 $ fromEnum 'A'
foodParseTag   = intToWord8 $ fromEnum 'F'
deathParseTag  = intToWord8 $ fromEnum 'D'
numberParseTag = intToWord8 $ fromEnum 'N'

parseGameData :: MutablePlayerList -> MutableFoodList -> [Word8] -> IO ()
parseGameData players foods (x:xs) =
  if | x == playerParseTag -> do
         let len = head xs
      -- parsePlayer players (tail xs)!!len
         parseGameData players foods $  drop (fromEnum len+1) xs
     | x == foodParseTag -> do
         let idx = take 2 xs
             len = xs !! 2
      -- parseFood foods idx (drop (2+1) xs)!!(len*2)
         parseGameData players foods $ drop (fromEnum len*2+1+2) xs
     | x == actionParseTag -> do
         let len = head xs
      -- parseAction take (len*2) (tail xs)
         parseGameData players foods $ drop (fromEnum len*2+1) xs
     | x == numberParseTag -> do
      -- parseNumber head xs
         parseGameData players foods $ tail xs
parseGameData _ _ [] = error "Caught data with empty"
