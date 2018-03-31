{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
module Main (main) where

import           Control.Monad         (when)
import qualified Data.ByteString       as B
import           Data.IORef
import           Data.List             (elemIndices)
import           Data.Semigroup        ((<>))
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import           Data.Word
import           System.IO             (hFlush, stdout, stdin, hSetEcho, hGetEcho)
import           System.IO.Unsafe      (unsafePerformIO)
import           System.Process        (createProcess, shell)

import Network.Socket hiding (recv, send)
import Network.Socket.ByteString (send)
import Options.Applicative

import Framework
import Mimizu

data ReadingSystem = Bouyomi | Softalk deriving (Show, Read, Eq)

data Options = Options
               { reader      :: ReadingSystem
               , softalkPath :: String
               , bouyomiHost :: String
               , bouyomiPort :: Int
#ifdef DEBUG
               , connectTo   :: String
               , isChatOnly  :: Bool
#endif
               }
{-
data Options = Options
               { reader      :: ReadingSystem
               , softalkPath :: String
               , bouyomiHost :: String
               , bouyomiPort :: Int
               , connectTo   :: String
               , isChatOnly  :: Bool
               }
-}

parseOptions :: Parser Options
parseOptions = Options
  <$> option auto
  ( long "reading-system"
  <> short 'r'
  <> value Softalk
  <> showDefault
  <> metavar "Bouyomi|Softalk"
  <> help "Specify which reading sytem to use")
  <*> option str
  ( long "softalk-path"
    <> short 's'
    <> metavar "/path/to/softalk"
    <> showDefault
    <> value "softalk.exe"
    <> help "Target for greeting" )
  <*> option str
  ( long "bouyomi-host"
  <> short 'b'
  <> metavar "HOSTADDR"
  <> value "127.0.0.1"
  <> showDefault
  <> help "Host address to Bouyomi-chan")
  <*> option auto
  ( long "bouyomi-port"
  <> short 'p'
  <> metavar "HOSTPORT"
  <> value 50001
  <> showDefault
  <> help "Port to Bouyomi-chan")
#ifdef DEBUG
  <*> option str
  ( long "game-connect-to"
    <> metavar "GAMEIP"
    <> short 'g'
    <> showDefault
    <> value "160.16.82.222"
    <> help "IP addres, the game/the chat connecting to")
  <*> switch
  ( long "chat-only"
    <> short 'c'
    <> help "Specifies whether chat only or not")
#endif

{-# NOINLINE options #-}
options :: IORef Options
options = unsafePerformIO $ newIORef (Options Softalk
                                       "softalk.exe"
                                       "127.0.0.1"
                                       50001
#ifdef DEBUG
                                       "160.16.82.222" False
#endif
                                     )

errorHandler :: ErrorHandler
errorHandler = putStrLn

chatCallback :: ChatCallback
chatCallback = do
  cs <- chats
  opt <- readIORef options
  if null cs
    then return ()
    else let dat = generateBouyomiData $ last cs
         in sendToSoftalk (softalkPath opt) dat
  return $ T.pack ""

chatCallback' :: ChatCallback
chatCallback' = return $ T.pack ""

chatReceived :: IO ()
chatReceived = do
  _chats <- chats
  when (null _chats) (return ())
  let dat = generateBouyomiData $ last _chats
  opt <- readIORef options
  if reader opt == Softalk
    then sendToSoftalk (softalkPath opt) dat
    else actionOnBouyomi (bouyomiHost opt) (bouyomiPort opt) $ \sock -> do  _ <- send sock dat
                                                                            return ()
  return ()

resolve :: HostName -> ServiceName -> IO AddrInfo
resolve host port = do
  let hints = defaultHints { addrSocketType = Stream }
  addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
  return addr

open :: AddrInfo -> IO Socket
open addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock $ addrAddress addr
  return sock

actionOnBouyomi :: String -> Int -> (Socket -> IO ()) -> IO ()
actionOnBouyomi host port action' = do
  addr <- resolve host (show port)
  sock <- open addr
  action' sock
  close sock

sendToSoftalk :: FilePath -> B.ByteString -> IO ()
sendToSoftalk spath (T.unpack . TE.decodeUtf8 . B.drop 15 -> msg) = do
  let process = shell $ spath ++ " /W:" ++ msg
  _<- createProcess process
  return ()

getPassword :: IO String
getPassword = do
  putStr "Password: "
  hFlush stdout
  _oldEcho <- hGetEcho stdin
  hSetEcho stdin False
  pass <- getLine
  let idx = elemIndices '\NAK' pass
      pass' = if null idx
              then pass
              else drop (last idx + 1) pass
  hSetEcho stdin _oldEcho
  return pass'

-- これもフォーマット させたいが,今は時間がないので, とりあえず 名前とメッセージだけを送信!
generateBouyomiData :: Chat -> B.ByteString
generateBouyomiData chat = B.pack $ sendCommand ++ defaultSpeed ++ defaultPitch
                           ++ defaultVolume ++ defaultTone ++ defaultEncoding
                           ++ messageLength ++ messageString
  where sendCommand   = [0x01, 0x00]
        defaultSpeed  = [0xff, 0xff]
        defaultPitch  = [0xff, 0xff]
        defaultVolume = [0xff, 0xff]
        defaultTone   = [0x00, 0x00]
        defaultEncoding = [0x00]
        messageLength = conv32To8 . toEnum $ length messageString
        messageString = B.unpack . TE.encodeUtf8 . T.pack $ sender chat ++ "\t" ++ message chat

sampleGameReceive :: GameReceiveCallback
sampleGameReceive = return Nothing

main :: IO ()
main = do
  let hdr = info (parseOptions <**> helper)
        ( fullDesc
          <> progDesc "Sending comment acquired from TUGame Chat stream to appropriate reader"
          <> header "chat - a simple comment reader bridge" )
  opt <- execParser hdr
  atomicModifyIORef' options $ const (opt, ())
  opt' <- readIORef options
  putStr "PID: "
  hFlush stdout
  pid <- getLine
#ifdef DEBUG
  let host  = hFunc . map ((read :: String -> Word8) . T.unpack) $ T.split (== '.') $ T.pack (connectTo opt')
      cFunc = if isChatOnly opt' then Just chatReceived else Nothing
#else
  let host = (160, 16, 82, 222)
      cFunc = Just chatReceived
#endif
  if not (null pid)
    then mainLoop pid errorHandler sampleGameReceive chatCallback chatCallback' False
#ifdef DEBUG
         (isChatOnly opt')
#else
         False
#endif
         (Just host)
         Nothing
         Nothing
         cFunc
    else do putStr "ID: "
            hFlush stdout
            id' <- getLine
            password <- getPassword
            putStrLn password
            mainLoop "" errorHandler sampleGameReceive chatCallback chatCallback' False
#ifdef DEBUG
              (isChatOnly opt')
#else
              False
#endif
              (Just host)
              (Just id')
              (Just password)
              cFunc
            -- chatOnly id' password chatReceived
  return ()
  where hFunc [a, b, c, d] = (a, b, c, d)
