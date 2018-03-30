{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Control.Exception     as E
import           Control.Monad         (forever, when)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C
import           Data.IORef
import           Data.List             (elemIndices)
import           Data.Semigroup        ((<>))
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import           Data.Time.Clock.POSIX (getCurrentTime)
import           System.IO             (hFlush, stdout, stdin, hSetEcho, hGetEcho)
import           System.IO.Unsafe      (unsafePerformIO)
import           System.Process        (createProcess, shell)

import Network.Socket hiding (recv, send)
import Network.Socket.ByteString (recv, send)
import Options.Applicative


import Framework
import Mimizu

data ReadingSystem = Bouyomi | Softalk deriving (Show, Read, Eq)

data Options = Options
               { reader      :: ReadingSystem
               , softalkPath :: String
               , bouyomiHost :: String
               , bouyomiPort :: Int
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

-- will be optparsed later.... may be
options :: IORef Options
options = unsafePerformIO $ newIORef (Options Bouyomi "softalk.exe" "127.0.0.1" 50001)

chatReceived :: IO ()
chatReceived = do
  _chats <- chats
  let dat = generateBouyomiData $ last _chats
  when (null _chats) (return ())
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
sendToSoftalk spath msg = do
  let str = B.drop 15 msg
  let process = shell (spath ++ " /W:" ++ (T.unpack .TE.decodeUtf8 $ str))
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

-- これも,もっとやりたいが,今は時間がないので, とりあえず 名前とメッセージだけを送信!
-- もっと とはフォーマット させたい。
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
        messageString = B.unpack . TE.encodeUtf8 . T.pack $ sender chat ++ "\t" ++ message chat -- ちょっと効率悪いかな？

main :: IO ()
main = do
--  curr <- getCurrentTime
  let hdr = info (parseOptions <**> helper)
        ( fullDesc
          <> progDesc "Sending comment acquired from TUGame Chat stream to appropriate reader"
          <> header "chat - a simple comment reader bridge" )
  opt <- execParser hdr
  atomicModifyIORef' options $ const (opt, ())
  putStr "ID: "
  hFlush stdout
  id' <- getLine
  password <- getPassword
  putStrLn password
  chatOnly id' password chatReceived
  return ()
