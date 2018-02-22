{-# LANGUAGE MultiWayIf #-}
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

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad (forever, replicateM)
import qualified Data.ByteString  as BS
import           Data.ByteString.Lazy (unpack, toStrict)
import           Data.Bits (shiftL, (.&.), (.|.))
import           Data.IORef
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Data.Word

import Network.Socket
import Network.WebSockets

import Mimizu

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

-- | The callback function, this is called when message from 'gamePort'.
type GameReceiveCallback =
  Index -- ^ The index of player you are currently playing.
  -> [(Index, Maybe Player)] -- ^ Current list of players, which may not be updated depending on the distance from you.
  -> [(Index, FoodBlock)] -- ^ Current list of food blocks, which may not be updated for the block where the block you are not in.
  -> IO (DashFlag, GameAngle) -- ^ The action you intend to be taken.

-- | Handler.
type ErrorHandler = Index -> [(Index, Maybe Player)] -> [(Index, FoodBlock)] -> String -> IO ()

-- | Must be in range 0 ~ 4096.
type GameAngle = Word16
-- | True if dash is on, False otherwise.
type DashFlag  = Bool

-- consider to use StorableArray Index a
-- | A list\/map of players, Nothing if the player for that Index is not being played\/dead.
type MutablePlayerList = [(Index, IORef (Maybe Player))]

-- | A list\/map of foods, Index represents the block.
type MutableFoodList   = [(Index, IORef FoodBlock)]

-- | Your ID for playing, the index is the key of MutablePlayerList.
type PlayerID          = IORef Index

-- | Convert to websocket acceptable form.
convertToSendable :: (DashFlag, GameAngle) -> BS.ByteString
convertToSendable (flg, ang) = BS.pack $ conv16To8 $ (ang::Word16) .&. 0xfff .|. ((if flg then 1 else 0) `shiftL` 15)

-- | The actual websocket handling function.
run :: ErrorHandler -> GameReceiveCallback -> ClientApp ()
run handler cb conn = do
  putStrLn "mainLoop Begins Here"
  players <- zip [0::Index ..] <$> replicateM 0x100 (newIORef Nothing)           :: IO MutablePlayerList
  foods    <- zip [0::Index ..] <$> replicateM 0x10000 (newIORef (FoodBlock [])) :: IO MutableFoodList
  frameCount <- newIORef 0 :: IO (IORef Word16)
  playerIndex <- newIORef 0 :: IO PlayerID
  putStrLn "data for the game initialized"
  sendTextData conn $ T.pack "Your PID
  putStrLn "My pid sent"
  _ <- forkIO $ forever $ do
    msg <- receiveDataMessage conn
    putStrLn $ "Message received: " ++ show msg
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
      Right _        -> do
        act' <- cb pid  players' foods'
        --sendBinaryData conn . convertToSendable =<< cb pid players' foods'
        sendBinaryData conn $ convertToSendable act'
        putStrLn $ "Binary data send: " ++ show act'
  forever $ threadDelay 1000

-- | Entry point
mainLoop :: ErrorHandler -> GameReceiveCallback -> IO ()
mainLoop handler cb = withSocketsDo $ runClient hostString (fromEnum gamePort) "/" (run handler cb)
  where hostString = toAddrString hostAddress


-- | Convert mutable list\/map to immutable
pullIORefs :: [(Index, IORef a)] -> IO [(Index, a)]
pullIORefs = mapM eliminateIORef

-- | Remove IORef out of the argument.
eliminateIORef :: (Index, IORef a) -> IO (Index, a)
eliminateIORef (idx, ref) = (idx,) <$> readIORef ref

parsePlayer, parseAction  :: MutablePlayerList -> Index ->  [Word8] -> IO ()

-- | Modify player list using pased action binary data for the specific player
parseAction lst idx d = do
  v <- readIORef ref
  case v of
    Nothing -> atomicWriteIORef ref . Just $ Player [] "" 0 val []
    Just _  -> atomicModifyIORef' ref (\r -> (modifyAction val r, ()))
  where (Just ref) = lookup idx lst
        val        = conv8To16 d

-- | Modify player list using parsed player other info for the specific plaeyr
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

-- | Modify ist using parsed index value
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
         putStrLn $ "parsePlayerTag: " ++ show idx ++ "," ++ show len ++ "," ++ show len'
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

-- | parse text data from 'gameSocket'
parseSkinData :: MutablePlayerList -> T.Text -> IO (Either String ())
parseSkinData lst txt = do
  let triplets = makeTriplets txt
  mapM_ (modifyNameSkin lst) triplets
  return $ Right ()

modifyNameSkin :: MutablePlayerList -> (Index, String, [Color]) -> IO ()
modifyNameSkin lst (idx, n, s) = do
  let Just ref = lookup idx lst
  v <- readIORef ref
  case v of
    Nothing -> atomicWriteIORef ref . Just $ Player s n 0 0 []
    Just _  -> do
      atomicModifyIORef' ref (\r -> (modifyName n r,()))
      atomicModifyIORef' ref (\r -> (modifySkin s r, ()))
