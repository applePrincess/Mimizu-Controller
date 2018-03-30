{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Control.Exception     as E
import           Control.Monad         (forever, when)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import           Data.Time.Clock.POSIX (getCurrentTime)
import           System.Environment    (getArgs)
import           System.IO             (hFlush, stdout, stdin, hSetEcho, hGetEcho)
import           System.IO.Unsafe      (unsafePerformIO)
import           System.Process        (createProcess, shell)

import Network.Socket hiding (recv, send)
import Network.Socket.ByteString (recv, send)

import Framework
import Mimizu

chatReceived :: IO ()
chatReceived = do
  _chats <- chats
  -- ハイスコアだけが送られている可能性もあるので, 注意。
    -- 現状はハイスコアは送られてないと仮定する
  let dat = generateBouyomiData $ last _chats
  actionOnBouyomi $ \sock -> do
    _ <- send sock dat
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

actionOnBouyomi :: (Socket -> IO ()) -> IO ()
actionOnBouyomi action' = do
  addr <- resolve "127.0.0.1" "50001" -- will be softcoded
  sock <- open addr
  action' sock
  close sock

sentToSoftalk :: B.ByteString -> IO ()
sentToSoftalk msg = do
  let str = B.drop 15 msg -- since we want to share headers to BouyomiChan
  let process = shell ("D:/yharuhi39/Downloads/softalk/softalk.exe /W:" ++ (T.unpack .TE.decodeUtf8 $ str))
  _<- createProcess process
  return () -- enforce strict evaluation

getPassword :: IO String
getPassword = do
  putStr "Password: "
  hFlush stdout
  _oldEcho <- hGetEcho stdin
  hSetEcho stdin False
  pass <- getLine
  hSetEcho stdin _oldEcho
  return pass

-- これも,もっとやりたいが,今は時間がないので, とりあえず 名前とメッセージだけを送信!
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
        messageString = B.unpack . TE.encodeUtf8 . T.pack $ (sender chat) ++ "\t" ++ (message chat) -- ちょっと効率悪いかな？

-- とりあえず第一引数がSoftalk だったらsoftalkに切り替える
main :: IO ()
main = do
  _args <- getArgs
  curr <- getCurrentTime
  let c = Chat  TUGame  curr "りんご姫" "今日もいいペンキ☆"
      cc = generateBouyomiData c
  sentToSoftalk cc
--  if length _args == 1 && head _args == "softalk"
--    then
--  putStrLn "ID? "
--  id <- getLine
--  password <- getPassword
--  chatOnly id password chatReceived
--  putStrLn $ '\t':(id ++ "\t" ++ password)
  return ()
  where
