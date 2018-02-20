{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
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

type GameAngle = Word16 -- ^ Must be in range 0 ~ 4096
type DashFlag  = Bool   -- ^ True if dash is on, False otherwise

type GameReceiveCallback  = Index -> [(Index, Player)] -> [(Index, FoodBlock)] -> (DashFlag, GameAngle)

-- consider to use StorableArray Index a
type MutablePlayerList = [(Index, IORef Player)]
type MutableFoodList   = [(Index, IORef FoodBlock)]
type PlayerID          = IORef Index

pullIORefs :: [(Index, IORef a)] -> IO [(Index, a)]
pullIORefs = mapM eliminateIORef

{-
run :: GameReceiveCallback -> ClientApp ()
run cb conn = do
  players <- newIORef [] :: IO MutablePlayerList
  foods <- newIORef [] :: IO MutableFoodList
  putStrLn " Game socket: Connected"
  sendData conn yourPID
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
eliminateIORef :: (Index, IORef a) -> IO (Index, a)
eliminateIORef (idx, ref) = fmap (idx,) (readIORef ref)

parsePlayer, parseAction, parseDeath  :: MutablePlayerList -> Index ->  [Word8] -> IO ()
parseAction = undefined
parseDeath = undefined
parsePlayer = undefined

parseFood :: MutableFoodList  -> Index -> [Word8] -> IO ()
parseFood = undefined

parseNumber :: IORef Index -> Word8 -> IO ()
parseNumber = undefined

-- ^ A tag for parsing data
playerParseTag, actionParseTag, foodParseTag, deathParseTag, numberParseTag :: Word8

playerParseTag = intToWord8 $ fromEnum 'P'
actionParseTag = intToWord8 $ fromEnum 'A'
foodParseTag   = intToWord8 $ fromEnum 'F'
deathParseTag  = intToWord8 $ fromEnum 'D'
numberParseTag = intToWord8 $ fromEnum 'N'

parseGameData :: PlayerID -> MutablePlayerList -> MutableFoodList -> [Word8] -> IO ()
parseGameData pid players foods (x:xs) =
  if | x == playerParseTag -> do
         let len = fromEnum $ head xs
             idx = integralToIndex $ xs !! 1
         parsePlayer players idx (take len (drop 2 xs))
         parseGameData pid players foods $  drop (fromEnum len+1) xs
     | x == foodParseTag -> do
         let len = fromEnum $ xs !! 2
             idx = integralToIndex . conv8To16 $ take 2 xs
         parseFood foods idx $ take (len*2) $ drop (2+1) xs
         parseGameData pid players foods $ drop (fromEnum len*2+1+2) xs
     | x == actionParseTag -> do
         let len = fromEnum $ head xs
             idx = integralToIndex $ xs !! 1
         parseAction players idx (take (len*2) (tail xs))
         parseGameData pid players foods $ drop (fromEnum len*2+1) xs
     | x == numberParseTag -> do
         parseNumber pid (head xs)
         parseGameData pid players foods $ tail xs
parseGameData _ _ _ [] = error "Caught data with empty"
