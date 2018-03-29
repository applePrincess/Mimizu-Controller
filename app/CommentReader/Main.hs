{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Control.Exception     as E
import           Control.Monad         (forever, when)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C
import           Data.IORef
import           Data.Maybe
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import           System.IO             (hFlush, stdout, stdin, hSetEcho, hGetEcho)
import           System.IO.Unsafe      (unsafePerformIO)

import Network.Socket hiding (recv, send)
import Network.Socket.ByteString (recv, send)

import Framework
import Mimizu

{-# NOINLINE bouyomiChanSocket #-}
bouyomiChanSocket :: IORef (Maybe Socket)
bouyomiChanSocket = unsafePerformIO $ newIORef Nothing


chatReceived :: IO ()
chatReceived = do
  _chats <- chats
  -- ハイスコアだけが送られている可能性もあるので, 注意。
  sock <- readIORef bouyomiChanSocket
  when (isJust sock) $ do
    -- 現状はハイスコアは送られてないと仮定する
    let dat = generateBouyomiData $ last _chats
    _ <- send (fromJust sock) dat
    return ()
  return ()

openBouyomi :: IO ()
openBouyomi = do
  addr <- resolve "127.0.0.1" "50001" -- will be softcoded
  sock <- open addr
  atomicModifyIORef' bouyomiChanSocket (\x -> (Just sock, ()))
  where  resolve host port = do
           let hints = defaultHints { addrSocketType = Stream }
           addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
           return addr
         open addr = do
           sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
           connect sock $ addrAddress addr
           return sock

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
  where sendCommand   = [0x00, 0x01]
        defaultSpeed  = [0xff, 0xff]
        defaultPitch  = [0xff, 0xff]
        defaultVolume = [0xff, 0xff]
        defaultTone   = [0x00, 0x00]
        defaultEncoding = [0x00]
        messageLength = conv32To8 . toEnum $ length messageString
        messageString = B.unpack . TE.encodeUtf8 . T.pack $ (sender chat) ++ "\t" ++ (message chat) -- ちょっと効率悪いかな？

main :: IO ()
main = do
  putStrLn "ID? "
  id <- getLine
  password <- getPassword

  -- only care about 棒読みちゃん
  openBouyomi
--  chatOnly id password chatReceived
  putStrLn $ '\t':(id ++ "\t" ++ password)
  return ()
  where
