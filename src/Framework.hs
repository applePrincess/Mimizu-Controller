{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE TupleSections #-}
{-|
Module      : Framework
Copyright   : (c) Apple Princess 2018
License     : MIT
Maintainer  : Apple Princess
Stability   : experimental
Portability : portable
-}
module Framework
  ( hostAddress
  , gamePort
  , chatPort
  , Action
  , GameReceiveCallback
  , ErrorHandler
  , ChatCallback
  , GameAngle
  , DashFlag
  , MutablePlayerList
  , MutableFoodList
  , PlayerID
  , Ranking
  , convertToSendable
  , convertToSendableWithKey
--  , pullIORefs
--  , eliminateIORef
  , fromRadian
  , fromPosition
  , fromPositionf
  , players
  , foods
  , chats
  , ranking
  , teamID
  , playerID
  , frameCount
  , mainLoop ) where

import           Control.Concurrent   ( forkIO, threadDelay, MVar, newEmptyMVar
                                      , takeMVar, putMVar, forkFinally, killThread )
import           Control.Monad        (forever, replicateM, unless, when)
import           Data.Bits            (shiftL, (.&.), (.|.))
import qualified Data.ByteString      as BS
import           Data.ByteString.Lazy (toStrict, unpack)
import           Data.IORef
import           Data.Maybe           (fromJust, isJust)
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL
import           Data.Text.Encoding   (decodeUtf8)
import           Data.Word
import           System.IO.Unsafe

import           Network.Socket
import           Network.WebSockets

import           Mimizu

-- | The destination IP address the socket connects to, in the form of Word8 quadruplet.
hostAddress :: (Word8, Word8, Word8, Word8)
hostAddress = (160, 16, 82, 222)

-- | Convert Word8 quadruple to host string.
toAddrString :: (Word8, Word8, Word8, Word8) -> String
toAddrString (x, y, z, w) = show x ++ "." ++ show y ++ "." ++ show z ++ "." ++ show w

-- | The port number the socket accepts.
gamePort, chatPort :: PortNumber
gamePort = 8888
chatPort = 8891

data Arrow = LeftArrow | RightArrow | None
-- | The type represents an action.
type Action = (GameAngle, DashFlag)

-- | The callback function, this is called when message received from 'gamePort'.
type GameReceiveCallback =
  IO (Maybe Action) -- ^ The action you intend to be taken, if Nothing no actions will be takes.

-- | Handler.
type ErrorHandler =
  String -- ^ The message of error thrown
  -> IO ()

-- | The callback function, this is called when message received from 'chatPort'
type ChatCallback = IO T.Text -- ^ The message you want to send, if it is empty sending will not be fired.

-- | Must be in range 0 ~ 4095.
type GameAngle = Word16
-- | True if dash is on, False otherwise.
type DashFlag  = Bool

-- consider to use StorableArray Index a
-- | A list\/map of players, Nothing if the player for that Index is not being played\/dead.
type MutablePlayerList = [IORef (Maybe Player)]

-- | A list\/map of food blocks.
type MutableFoodList   = [IORef FoodBlock]

-- | Your ID for playing, the index is the key of MutablePlayerList.
type PlayerID          = IORef Index

-- | High score of the day (when you started to play).
type Ranking           = [(String, Word32)]

-- | Convert to websocket acceptable form.
convertToSendable :: Action -> BS.ByteString
convertToSendable (ang, flg) = BS.pack $ conv16To8 $ (ang::Word16) .&. 0xfff .|. ((if flg then 1 else 0) `shiftL` 15)

-- | Convert to websocket acceptable form, if we send by arrow keys.
convertToSendableWithKey :: Action -> Arrow -> BS.ByteString
convertToSendableWithKey (ang, flg) arr  = BS.pack $ case arr of
                                                       LeftArrow  -> [0x2 .|. hBit, lBit]
                                                       RightArrow -> [0x4 .|. hBit, lBit]
                                                       None       -> [hBit, lBit]
  where [hBit, lBit] = BS.unpack $ convertToSendable (ang, flg)

-- * The section of internal data.

{-# NOINLINE playersInternal #-}
playersInternal :: MutablePlayerList
playersInternal = unsafePerformIO $ replicateM 0x100 (newIORef Nothing)

{-# NOINLINE  foodsInternal #-}
foodsInternal :: MutableFoodList
foodsInternal = unsafePerformIO $ replicateM 0x10000 (newIORef (FoodBlock []))

{-# NOINLINE chatsInternal #-}
chatsInternal :: IORef [Chat]
chatsInternal = unsafePerformIO $ newIORef []

{-# NOINLINE rankingInternal #-}
rankingInternal :: IORef Ranking
rankingInternal = unsafePerformIO $ newIORef []

{-# NOINLINE teamIDInternal #-}
teamIDInternal :: IORef (Maybe Word8)
teamIDInternal = unsafePerformIO $ newIORef Nothing

{-# NOINLINE  playerIDInternal #-}
playerIDInternal :: IORef Index
playerIDInternal = unsafePerformIO $ newIORef 0

{-# NOINLINE  frameCountInternal #-}
frameCountInternal :: IORef Word16
frameCountInternal = unsafePerformIO $ newIORef 0

-- * External Info

-- | Current list of players, which may not be updated depending on the distance from you.
players :: IO [Maybe Player]
players = pullIORefs playersInternal

-- | Current list of food blocks, which may not be updated for the block where the block you are not in.
foods :: IO [FoodBlock]
foods = pullIORefs foodsInternal

-- |  Chats recieved since connected, including the latest.
chats :: IO [Chat]
chats = readIORef chatsInternal

-- | The ranking of the day.
ranking :: IO Ranking
ranking = readIORef rankingInternal

-- | Your team number.
teamID :: IO (Maybe Word8)
teamID = readIORef teamIDInternal

-- | The index of player you are currently playing.
playerID :: IO Index
playerID = readIORef playerIDInternal

-- | Frames passed since the server starts up, but only get lowest 16 bits.
frameCount :: IO Word16
frameCount = readIORef frameCountInternal

-- | Convert mutable list\/map to immutable.
pullIORefs :: [IORef a] -> IO [a]
pullIORefs = mapM readIORef

-- | Remove IORef out of the argument.

-- eliminateIORef :: (Index, IORef a) -> IO (Index, a)
-- eliminateIORef (idx, ref) = (idx,) <$> readIORef ref

-- | Convert radians (which must be in range (-pi, pi]) to sendable angle.
fromRadian :: Double -> GameAngle
fromRadian ang | ang < 0   = a .|. 0x1000
               | otherwise = a
  where a = floor ((ang / pi) * 0x800) .&. 0xfff

-- | Convert a vector x y to sendable angle.
fromPosition :: Int -> Int -> GameAngle
fromPosition x y = floor ((ang / pi) * 0x800) .&. 0xfff
  where ang = atan2 (fromIntegral y) (fromIntegral x)

-- | Same as 'fromPosition' but takes two 'Double's instead.
fromPositionf :: Double -> Double -> GameAngle
fromPositionf x y = floor ((ang / pi) * 0x800) .&. 0xfff
  where ang = atan2 y x

-- | A tag for parsing data.
playerParseTag, actionParseTag, foodParseTag, deathParseTag, numberParseTag :: Word8
frameCountParseTag :: Word8

playerParseTag     = intToWord8 $ fromEnum 'P'
actionParseTag     = intToWord8 $ fromEnum 'A'
foodParseTag       = intToWord8 $ fromEnum 'F'
deathParseTag      = intToWord8 $ fromEnum 'D'
numberParseTag     = intToWord8 $ fromEnum 'N'
frameCountParseTag = intToWord8 $ fromEnum 'Z'
--teamParserTag  = intToWord8 $ fromEnum 'T' -- for future use.

parsePlayer, parseFood, parseAction :: Index -> [Word8] -> IO ()
parseNumber, parseDeath :: Index -> IO ()
parseFrameCount :: [Word8] -> IO ()

-- | Modify player list using parsed player other info for the specific plaeyr.
parsePlayer idx d = do
  let pRef = playersInternal !! fromEnum idx
  pl <- readIORef pRef
  case pl of
    Nothing -> atomicWriteIORef pRef . Just $ createPlayer d
    Just _  -> atomicModifyIORef' pRef (\r -> (modifyPlayerInfo d r, ()))

-- | Modify food list using parsed food info.
parseFood idx d = atomicWriteIORef ref $ FoodBlock d
  where ref = foodsInternal !! fromEnum idx

-- | Modify player list using pased action binary data for the specific player.
parseAction idx d = do
  let pRef = playersInternal !! fromEnum idx
      val  = conv8To16 d
  pl <- readIORef pRef
  case pl of
    Nothing -> error $ "Unrecognized player's action found: " ++ show d
    Just _  -> atomicModifyIORef' pRef (\r -> (modifyAction val r, ()))

-- | Modify ist using parsed index value.
parseNumber idx = atomicWriteIORef playerIDInternal (integralToIndex idx)

-- | Modify player list using parsed death info
parseDeath idx = atomicWriteIORef ref Nothing
  where ref = playersInternal !! fromEnum idx

parseFrameCount = writeIORef frameCountInternal . conv8To16

parseGameData :: [Word8] -> IO (Either String ())
parseGameData (x:xs) =
  if | x == playerParseTag -> do
         let idx = integralToIndex $ head xs
             len = fromEnum $ xs !! 1
             len' = if len > 0 then (len + 1) * 8 + 6 else 8
         parsePlayer idx . take len' $ drop 2 xs
         parseGameData $ drop (fromEnum len' + 2) xs
     | x == foodParseTag -> do
         let idx = integralToIndex . conv8To16 $ take 2 xs
             len = fromEnum $ xs !! 2
         parseFood idx . take (len * 2) $ drop (2+1) xs
         parseGameData $ drop (fromEnum len * 2) xs
     | x == actionParseTag -> do
         let idx = integralToIndex $ head xs
         parseAction idx (take 2 (tail xs))
         parseGameData $ drop (2 + 1) xs
     | x == numberParseTag -> do
         parseNumber . integralToIndex $ head xs
         parseGameData $ tail xs
     | x == deathParseTag -> do
         let idx = integralToIndex . fromEnum $ head xs
         pid <- readIORef playerIDInternal
         if pid == idx
           then return $ Left "You are Dead"
           else do parseDeath idx
                   parseGameData $ tail xs
     | x == frameCountParseTag -> do
         parseFrameCount $ take 2 xs
         parseGameData $ drop 2 xs
--     | x == teamParseTag -> do parseGameData xs
parseGameData [] = return $ Right ()

-- | Modify player list using parsed text data from 'gameSocket'.
parseSkinData :: T.Text -> IO (Either String ())
parseSkinData txt = do
  let triplets = makeTriplets txt
  mapM_ parseNameSkin triplets
  return $ Right ()

-- | Modify player specified by index in player list using specified tuple.
parseNameSkin :: (Index, String, [Color]) -> IO ()
parseNameSkin (idx, n, s) = do
  let ref =  playersInternal !! fromEnum idx
  v <- readIORef ref
  case v of
    Nothing -> atomicWriteIORef ref . Just $ Player s n 0 0 []
    Just _  -> do
      atomicModifyIORef' ref (\r -> (modifyName n r,()))
      atomicModifyIORef' ref (\r -> (modifySkin s r, ()))

-- | Parse Text into Chat so that you can manimupate easily.
parseChat :: T.Text -> IO ()
parseChat msg = do
  utcTime  <- fromTimeString $ T.unpack ts
  let chat = Chat (fromOriginString $ T.unpack orig) utcTime
             (T.unpack n) (T.unpack $ T.concat ms)
  atomicModifyIORef' chatsInternal (\x -> (x ++ [chat], ()))
  where (orig:ts:n:ms)  = T.splitOn (T.pack "\t") msg

-- | Parse Text into Ranking.
parseRanking :: T.Text -> IO () -- Ranking
parseRanking msg = atomicWriteIORef rankingInternal rankingRef
  where parseRank (rankedName:score:others) = (rankedName, read score) : parseRank others
        parseRank [_]                 = error $ "Unrecognized hiscore string found: " ++ show msg
        parseRank []                  = []
        rankingRef                    = parseRank . tail . words $ T.unpack msg

-- | Select team and send it to the socket.
selectTeam :: Connection -> IO (Either String ())
selectTeam conn = do
  putStrLn "0: 青(Blue)"
  putStrLn "1: 赤(Red)"
  putStrLn "2: 緑(Green)"
  putStrLn "3: 黄(Yellow)"
  -- TODO: Error checking, other than 1-4 input the program will soon throw exception and crash.
  choice <- (read :: String -> Word8) <$> getLine
  if choice < 0 || choice > 3
    then do putStrLn "Please input in coreet team number"
            selectTeam conn
    else do writeIORef teamIDInternal $ Just choice
            sendBinaryData conn $ BS.pack [choice]
            return $ Right ()

-- | The actual websocket handling function for 'gamePort'.
run :: String -> ErrorHandler -> GameReceiveCallback -> MVar () -> ClientApp ()
run sid handler cb mv conn = do
  sendTextData conn $ T.pack sid
  mv' <- newEmptyMVar
  _ <- forkIO $ forever $ do
    msg <- receiveDataMessage conn
    val <- case msg of
             (Text t txt) -> if txt == (Just $ TL.pack "T")
                             then selectTeam conn
                             else parseSkinData (decodeUtf8 (toStrict t))
             (Binary bs) -> parseGameData $ unpack bs
    case val of
      Left errString -> do
        handler errString
        putMVar mv' ()
      Right _        -> do
        action <- cb
        when (isJust action) $ sendBinaryData conn $ convertToSendable (fromJust action)
  takeMVar mv'
  putMVar mv ()

-- | The actual websocket handling functin for 'chatPort'.
runChat :: String -> ChatCallback -> ChatCallback -> ClientApp ()
runChat sid cb chatSend conn = do
  sendTextData conn . T.pack $ sid ++ "\tCHAT"
  receiveData conn >>= parseRanking
  _ <- forkIO $ forever $ do
   receiveData conn >>= parseChat
   sendChat cb
  forever $ do
    sendChat chatSend
    threadDelay 1000
  where sendChat :: ChatCallback -> IO ()
        sendChat cb' = do
          newChat <- cb'
          unless (T.null newChat) $ sendTextData conn newChat

-- | Entry point.
mainLoop :: String -- ^ The session id you want to play.
  -> ErrorHandler -- ^ The callback, which will be called when error occured.
  -> GameReceiveCallback -- ^ The callback, which will be called when any new message from game received.
  -> ChatCallback -- ^ The callback, which will be called when any new message from chat received.
  -> ChatCallback -- ^ The function, which will be called every 1s, so that you can actively send.
  -> Bool         -- ^ The flag, True if run this thread again, False otherwise.
  -> IO ()
mainLoop sid handler cbGame cbChat chatSend isForever = withSocketsDo $ do
  mv <- newEmptyMVar
  chatThreadID <- forkFinally (startClient chatPort (runChat sid cbChat chatSend)) (\_ -> return ())
  _ <- forkFinally (gameLoop sid handler cbGame isForever mv) (\_ -> return ())
  _ <- takeMVar mv
  killThread chatThreadID
  where hostString = toAddrString hostAddress
        startClient pt = runClient hostString (fromEnum pt) "/"

-- need to think the way to get there.
gameLoop :: String -> ErrorHandler -> GameReceiveCallback -> Bool -> MVar () -> IO ()
gameLoop sid handler cb isForever mv' = do
  mv <- newEmptyMVar
  gameThreadID <- forkFinally (runClient hostString (fromEnum gamePort) "/" (run sid handler cb mv)) (\_ -> return ())
  takeMVar mv
  killThread gameThreadID
  when isForever $ do
    threadDelay (5 * 10^6) -- wait a second. so that the server will not be busy when the program connect again.
    gameLoop sid handler cb isForever mv'
  putMVar mv' ()
  where  hostString = toAddrString hostAddress
