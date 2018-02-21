{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
module Framework where

import           Control.Concurrent (forkIO)
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
  -> (DashFlag, GameAngle) -- ^ The action you intend to be taken.

type GameAngle = Word16 -- ^ Must be in range 0 ~ 4096.
type DashFlag  = Bool   -- ^ True if dash is on, False otherwise.

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


run :: GameReceiveCallback -> ClientApp ()
run cb conn = do
  players <- zip [1::Index ..] <$> replicateM 0x100 (newIORef Nothing)           :: IO MutablePlayerList
  foods    <- zip [1::Index ..] <$> replicateM 0x10000 (newIORef (FoodBlock [])) :: IO MutableFoodList
  playerIndex <- newIORef 0 :: IO PlayerID
  sendTextData conn $ T.pack "yourPID"
  _ <- forkIO $ forever $ do
    msg <- receiveDataMessage conn
    case msg of
      (Text t _)-> parseSkinData players (decodeUtf8 (toStrict t))
      (Binary bs) -> parseGameData playerIndex players foods $ unpack bs
    players' <- pullIORefs players
    foods'   <- pullIORefs foods
    pid      <- readIORef playerIndex
    sendBinaryData conn . convertToSendable $ cb pid players' foods'
  forever (return ())

mainLoop :: GameReceiveCallback -> IO ()
mainLoop cb = withSocketsDo $ runClient hostString (fromEnum gamePort) "/" (run cb)
  where hostString = toAddrString hostAddress


-- | Convert mutable list\/map to immutable
pullIORefs :: [(Index, IORef a)] -> IO [(Index, a)]
pullIORefs = mapM eliminateIORef

eliminateIORef :: (Index, IORef a) -> IO (Index, a)
eliminateIORef (idx, ref) = fmap (idx,) (readIORef ref)

-- | parse specific binary data
parsePlayer, parseAction  :: MutablePlayerList -> Index ->  [Word8] -> IO ()

parseAction lst idx d = modifyIORef ref (modifyAction val)
  where (Just ref) = lookup idx lst
        val        = conv8To16 d

parsePlayer lst idx d = modifyIORef ref (modifyPlayerInfo d)
  where Just ref = lookup idx lst

parseDeath :: MutablePlayerList -> Index -> IO ()
parseDeath lst idx = writeIORef ref Nothing
  where Just ref = lookup idx lst

parseFood :: MutableFoodList  -> Index -> [Word8] -> IO ()
parseFood lst idx d = writeIORef ref $ FoodBlock d
  where Just ref = lookup idx lst

parseNumber :: IORef Index -> Word8 -> IO ()
parseNumber ref idx = writeIORef ref (integralToIndex idx)

-- | A tag for parsing data
playerParseTag, actionParseTag, foodParseTag, deathParseTag, numberParseTag :: Word8

playerParseTag = intToWord8 $ fromEnum 'P'
actionParseTag = intToWord8 $ fromEnum 'A'
foodParseTag   = intToWord8 $ fromEnum 'F'
deathParseTag  = intToWord8 $ fromEnum 'D'
numberParseTag = intToWord8 $ fromEnum 'N'

-- | parse binary data from 'gameSocket'
parseGameData :: PlayerID -> MutablePlayerList -> MutableFoodList -> [Word8] -> IO ()
parseGameData pid players foods (x:xs) =
  if | x == playerParseTag -> do
         let len = fromEnum $ head xs
             idx = integralToIndex $ xs !! 1
         parsePlayer players idx (take len (drop 2 xs))
         parseGameData pid players foods $  drop (fromEnum len+1) xs
     | x == foodParseTag -> do
         let idx = integralToIndex . conv8To16 $ take 2 xs
             len = fromEnum $ xs !! 2
         parseFood foods idx . take (len*2) $ drop (2+1) xs
         parseGameData pid players foods $ drop (fromEnum len*2+1+2) xs
     | x == actionParseTag -> do
         let len = fromEnum $ head xs
             idx = integralToIndex $ xs !! 1
         parseAction players idx (take (len*2) (tail xs))
         parseGameData pid players foods $ drop (fromEnum len*2+1) xs
     | x == numberParseTag -> do
         parseNumber pid (head xs)
         parseGameData pid players foods $ tail xs
     | x == deathParseTag -> do
         let idx = integralToIndex . fromEnum $ head xs
         parseDeath players idx
         parseGameData pid players foods $ tail xs

parseGameData _ _ _ [] = return ()


-- | parse text data from 'gameSocket'
parseSkinData :: MutablePlayerList -> T.Text -> IO ()
parseSkinData lst txt = do
  let triplets = makeTriplets txt
  -- assume the triplets are formed as [(Index, Name, Skin)]
  mapM_ (\(idx, n, s) -> do
            let ref = lookup idx lst
            case ref of
              Nothing -> return ()
              Just vRef -> do
                modifyIORef vRef (modifyName n)
                modifyIORef vRef (modifySkin s)) triplets
