{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
-- {-# LANGUAGE CPP #-}
module Main (main) where

import           Control.Monad             (when)
import qualified Data.ByteString           as B
import           Data.IORef
import           Data.List                 (elemIndices)
import           Data.Semigroup            ((<>))
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import           Data.Word
import           System.IO                 (hFlush, hGetEcho, hSetEcho, stdin,
                                            stdout)
import           System.IO.Unsafe          (unsafePerformIO)
import           System.Process            (createProcess, shell)

import           Network.Socket            hiding (recv, send)
import           Network.Socket.ByteString (send)
import           Options.Applicative

import           Framework
import           Mimizu

data ReadingSystem = Bouyomi | Softalk deriving (Show, Read, Eq)

data Options = Options
               { reader      :: ReadingSystem
               , softalkPath :: String
               , bouyomiHost :: String
               , bouyomiPort :: Int
               , identifier  :: String
               , password    :: String
               , formatStr   :: String
               }

parseOptions :: Parser Options
parseOptions = Options
  <$> option auto
  ( long "reading-system"
  <> short 'r'
  <> value Bouyomi
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
  <*> option str
  ( long "id"
  <> short 'i'
  <> metavar "ID"
  <> help "Your Desired ID to log in. (if not present, the program will ask  at the beginning.)"
  <> value "")
  <*> option str
  ( long "password"
  <> short 'w'
  <> help "The password, corresponding to ID specified. (if not present, the program will ask at the beginning.)"
  <> metavar "PASSWORD"
  <> value "")
  <*> option str
  ( long "format"
  <> short 'f'
  <> help "Specifies the format, which read by the Voice Synthesiser, %m: message, %s: sender"
  <> metavar "FORMAT"
  <> value "%s\t%m"
  <> showDefault )

{-# NOINLINE options #-}
options :: IORef Options
options = unsafePerformIO $ newIORef (Options
                                      Bouyomi
                                      "softalk.exe"
                                      "127.0.0.1"
                                      50001
                                      ""
                                      ""
                                      "%s\t%m"
                                     )

errorHandler :: ErrorHandler
errorHandler = putStrLn

chatCallback :: ChatCallback
chatCallback = do
  cs <- chats
  opt <- readIORef options
  if null cs
    then return ()
    else let dat = generateBouyomiData (last cs) (formatStr opt)
         in sendToSoftalk (softalkPath opt) dat
  return $ T.pack ""

chatCallback' :: ChatCallback
chatCallback' = return $ T.pack ""

chatReceived :: IO ()
chatReceived = do
  _chats <- chats
  when (null _chats) (return ())
  opt <- readIORef options
  let dat = generateBouyomiData (last _chats) (formatStr opt)
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
  let process = shell $ spath ++ " /X:1 /W:" ++ msg
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


formatMessage :: Chat -> String -> T.Text
formatMessage c fStr = T.replace (T.pack "\\t") (T.pack "\t") $
                       T.replace (T.pack "%s") (T.pack $ sender c) $
                       T.replace (T.pack "%m") (T.pack $ message c) (T.pack fStr)

generateBouyomiData :: Chat -> String ->  B.ByteString
generateBouyomiData chat fStr = B.pack $ sendCommand ++ defaultSpeed ++ defaultPitch
                                ++ defaultVolume ++ defaultTone ++ defaultEncoding
                                ++ messageLength ++ messageString
  where sendCommand   = [0x01, 0x00]
        defaultSpeed  = [0xff, 0xff]
        defaultPitch  = [0xff, 0xff]
        defaultVolume = [0xff, 0xff]
        defaultTone   = [0x00, 0x00]
        defaultEncoding = [0x00]
        messageLength = conv32To8 . toEnum $ length messageString
        messageString = B.unpack . TE.encodeUtf8 $ formatMessage chat fStr

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
  let host = (160, 16, 82, 222)
      cFunc = Just chatReceived
  opt' <- readIORef options
  id' <- if null (identifier opt')
         then do putStr "ID: "
                 hFlush stdout
                 getLine
         else return (identifier opt')
  password' <- if null (password opt')
               then getPassword
               else return (password opt')
  mainLoop "" errorHandler sampleGameReceive chatCallback chatCallback' False
             True
              (Just host)
              (Just id')
              (Just password')
              cFunc
  where hFunc [a, b, c, d] = (a, b, c, d)
