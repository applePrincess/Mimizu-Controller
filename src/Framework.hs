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
module Framework where

import           Control.Concurrent   (forkIO, threadDelay)
import           Control.Monad        (forever, replicateM, unless)
import           Data.Bits            (shiftL, (.&.), (.|.))
import qualified Data.ByteString      as BS
import           Data.ByteString.Lazy (toStrict, unpack)
import           Data.IORef
import qualified Data.Text            as T
import           Data.Text.Encoding   (decodeUtf8)
import           Data.Word

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

-- | The callback function, this is called when message received from 'gamePort'.
type GameReceiveCallback =
  Index -- ^ The index of player you are currently playing.
  -> [(Index, Maybe Player)] -- ^ Current list of players, which may not be updated depending on the distance from you.
  -> [(Index, FoodBlock)] -- ^ Current list of food blocks, which may not be updated for the block where the block you are not in.
  -> IO (DashFlag, GameAngle) -- ^ The action you intend to be taken.

-- | Handler.
type ErrorHandler =
  Index -- ^ The index of player you are currently playing.
  -> [(Index, Maybe Player)] -- ^ Current list of players, which may not be updated depending on the distance from you.
  -> [(Index, FoodBlock)] -- ^ Current list of food blocks, which may not be updated for the block where the block you are not in.
  -> String -- ^ The message of error thrown
  -> IO ()

-- | The callback function, this is called when message received from 'chatPort'
type ChatCallback =
  [Chat] -- ^ Chats recieved, including the latest
  -> IO T.Text -- ^ The message you want to send, if it is empty sending will not be fired.

-- | Must be in range 0 ~ 4095.
type GameAngle = Word16
-- | True if dash is on, False otherwise.
type DashFlag  = Bool

-- consider to use StorableArray Index a
-- | A list\/map of players, Nothing if the player for that Index is not being played\/dead.
type MutablePlayerList = [(Index, IORef (Maybe Player))]

-- | A list\/map of food blocks.
type MutableFoodList   = [(Index, IORef FoodBlock)]

-- | Your ID for playing, the index is the key of MutablePlayerList.
type PlayerID          = IORef Index

-- | Convert to websocket acceptable form.
convertToSendable :: (DashFlag, GameAngle) -> BS.ByteString
convertToSendable (flg, ang) = BS.pack $ conv16To8 $ (ang::Word16) .&. 0xfff .|. ((if flg then 1 else 0) `shiftL` 15)

-- | Convert mutable list\/map to immutable.
pullIORefs :: [(Index, IORef a)] -> IO [(Index, a)]
pullIORefs = mapM eliminateIORef

-- | Remove IORef out of the argument.
eliminateIORef :: (Index, IORef a) -> IO (Index, a)
eliminateIORef (idx, ref) = (idx,) <$> readIORef ref

parsePlayer, parseAction  :: MutablePlayerList -> Index ->  [Word8] -> IO ()

-- | Modify player list using pased action binary data for the specific player.
parseAction lst idx d = do
  v <- readIORef ref
  case v of
    Nothing -> atomicWriteIORef ref . Just $ Player [] "" 0 val []
    Just _  -> atomicModifyIORef' ref (\r -> (modifyAction val r, ()))
  where (Just ref) = lookup idx lst
        val        = conv8To16 d

-- | Modify player list using parsed player other info for the specific plaeyr.
parsePlayer lst idx d = do
  v <- readIORef ref
  case v of
    Nothing -> atomicWriteIORef ref . Just $ createPlayer d
    Just _  -> atomicModifyIORef' ref (\r -> (modifyPlayerInfo d r, ()))
  where Just ref = lookup idx lst

-- | Modify player list using parsed death info
parseDeath :: MutablePlayerList -> Index -> IO ()
parseDeath lst idx = atomicWriteIORef ref Nothing
  where Just ref = lookup idx lst

-- | Modify food list using parsed food info.
parseFood :: MutableFoodList  -> Index -> [Word8] -> IO ()
parseFood lst idx d = atomicWriteIORef ref $ FoodBlock d
  where Just ref = lookup idx lst

-- | Modify ist using parsed index value.
parseNumber :: IORef Index -> Word8 -> IO ()
parseNumber ref idx = atomicWriteIORef ref (integralToIndex idx)

-- | A tag for parsing data.
playerParseTag, actionParseTag, foodParseTag, deathParseTag, numberParseTag :: Word8

playerParseTag = intToWord8 $ fromEnum 'P'
actionParseTag = intToWord8 $ fromEnum 'A'
foodParseTag   = intToWord8 $ fromEnum 'F'
deathParseTag  = intToWord8 $ fromEnum 'D'
numberParseTag = intToWord8 $ fromEnum 'N'

-- | parse binary data from 'gameSocket'.
parseGameData :: PlayerID -> MutablePlayerList -> MutableFoodList -> [Word8] -> IO (Either String ())
parseGameData pid players foods (x:xs) =
  if | x == playerParseTag -> do
         let idx = integralToIndex $ head xs
             len = fromEnum $ xs!!1
             len' = if len > 0 then (len + 1) * 8 + 6 else 8
         parsePlayer players idx . take len' $ drop 2 xs
         parseGameData pid players foods $  drop (fromEnum len'+2) xs
     | x == foodParseTag -> do
         let idx = integralToIndex . conv8To16 $ take 2 xs
             len = fromEnum $ xs !! 2
         parseFood foods idx . take (len*2) $ drop (2+1) xs
         parseGameData pid players foods $ drop (fromEnum len*2+1+2) xs
     | x == actionParseTag -> do
         let idx = integralToIndex $ head xs
         parseAction players idx (take 2 (tail xs))
         parseGameData pid players foods $ drop (2+1) xs
     | x == numberParseTag -> do
         parseNumber pid (head xs)
         parseGameData pid players foods $ tail xs
     | x == deathParseTag -> do
         let idx = integralToIndex . fromEnum $ head xs
         pid' <- readIORef pid
         if pid' == idx
           then return $ Left "You are dead."
           else (do
           parseDeath players idx
           parseGameData pid players foods $ tail xs)
     | otherwise         -> error $ "Unrecognized tag found: " ++ show x
parseGameData _ _ _ [] = return $ Right ()

-- | Modify player list using parsed text data from 'gameSocket'.
parseSkinData :: MutablePlayerList -> T.Text -> IO (Either String ())
parseSkinData lst txt = do
  let triplets = makeTriplets txt
  mapM_ (parseNameSkin lst) triplets
  return $ Right ()

-- | Modify player specified by index in player list using specified tuple.
parseNameSkin :: MutablePlayerList -> (Index, String, [Color]) -> IO ()
parseNameSkin lst (idx, n, s) = do
  let Just ref = lookup idx lst
  v <- readIORef ref
  case v of
    Nothing -> atomicWriteIORef ref . Just $ Player s n 0 0 []
    Just _  -> do
      atomicModifyIORef' ref (\r -> (modifyName n r,()))
      atomicModifyIORef' ref (\r -> (modifySkin s r, ()))


-- | Parse Text into Chat so that you can manimupate easily.
parseChat :: T.Text -> IO Chat
parseChat msg = do
  utcTime  <- fromTimeString $ T.unpack ts
  return $ Chat (fromOriginString $ T.unpack orig) utcTime
    (T.unpack n) (T.unpack $ T.concat ms)
  where (orig:ts:n:ms)  = T.splitOn (T.pack "\t") msg

-- | The actual websocket handling function for 'gamePort'.
run :: String -> ErrorHandler -> GameReceiveCallback -> ClientApp ()
run sid handler cb conn = do
  players <- zip [0::Index ..] <$> replicateM 0x100 (newIORef Nothing)           :: IO MutablePlayerList
  foods    <- zip [0::Index ..] <$> replicateM 0x10000 (newIORef (FoodBlock [])) :: IO MutableFoodList
  frameCount <- newIORef 0 :: IO (IORef Word16)
  playerIndex <- newIORef 0 :: IO PlayerID
  sendTextData conn $ T.pack sid
  _ <- forkIO $ forever $ do
    msg <- receiveDataMessage conn
    val <- case msg of
             (Text t _)  -> parseSkinData players (decodeUtf8 (toStrict t))
             (Binary bs) -> do
               let h2 = take 2 (unpack bs)
               writeIORef frameCount $ conv8To16 h2
               parseGameData playerIndex players foods . drop 2 $ unpack bs
    players' <- pullIORefs players
    foods'   <- pullIORefs foods
    pid      <- readIORef playerIndex
    case val of
      Left errString -> do
        handler pid players' foods' errString
        error "This socket is terminated."
      Right _        -> sendBinaryData conn . convertToSendable =<< cb pid  players' foods'
  forever $ threadDelay 1000

-- | The actual websocket handling functin for 'chatPort'.
runChat :: String -> ChatCallback -> ChatCallback -> ClientApp ()
runChat sid cb chatSend conn = do
  chats <- newIORef [] :: IO (IORef [Chat])
  sendTextData conn . T.pack $ sid ++ "\tCHAT"
  _ <- forkIO $ forever $ do
    msg <- receiveData conn :: IO T.Text
    c   <- parseChat msg
    atomicModifyIORef' chats (\x -> (x ++ [c], ()))
    sendChat chats cb
  forever $ do
    sendChat chats chatSend
    threadDelay 1000
  where sendChat :: IORef [Chat] -> ChatCallback -> IO ()
        sendChat chats cb' = do
          chats' <- readIORef chats
          newChat <- cb' chats'
          unless (T.null newChat) $ sendTextData conn newChat

-- | Entry point.
mainLoop :: String -- ^ The session id you want to play.
  -> ErrorHandler -- ^ The callback, which will be called when error occured.
  -> GameReceiveCallback -- ^ The callback, which will be called when any new message from game received.
  -> ChatCallback -- ^ The callback, which will be called when any new message from chat received.
  -> ChatCallback -- ^ The function, which will be called every 1s, so that you can actively send.
  -> IO ()
mainLoop sid handler cbGame cbChat chatSend = withSocketsDo $ do
  _ <- forkIO $ runClient hostString (fromEnum gamePort) "/" (run sid handler cbGame)
  _ <- forkIO $ runClient hostString (fromEnum chatPort) "/" (runChat sid cbChat chatSend)
  forever $ threadDelay 1000
  where hostString = toAddrString hostAddress
