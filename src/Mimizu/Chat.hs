{-|
Module      : Mimizu.Chat
Copyright   : (c) Apple Princess 2018
License     : MIT
Maintainer  : Apple Princess
Stability   : experimental
Portability : portable
-}

module Mimizu.Chat
  ( Chat(..)
  , Origin(..)
  , toOriginString
  , fromOriginString
  , fromTimeString )where

import           Data.List (unfoldr)

import           Data.Time.Clock


-- | The representation of one message
data Chat = Chat { origin  :: Origin  -- ^ The origin of message
                 , time    :: UTCTime   -- ^ The time (Internally in UTC, Externally in JST)
                 , sender  :: String  -- ^ The display name of sender the message sent by
                 , message :: String -- ^ The actual message
                 } deriving Show

-- | The representation of origin where a message come from
data Origin = NicoNico -- ^ NicoNico live stream. See <http://com.nicovideo.jp/community/co3265652>
            | TUGame   -- ^ TUGame original site. See <http://tk2-217-18218.vs.sakura.ne.jp>
            | YouTube  -- ^ YouTube live stream. See <https://www.youtube.com/channel/UCzXL5v5_-L-s4cDhK3I35Dg>
            | TwitCasting -- ^ Twitcasting live stream. See <https://twitcasting.tv/c:t_umezawa>
            deriving Show

-- | convert origin to chat websocket acceptable string
toOriginString :: Origin -> String
toOriginString NicoNico    = "NN"
toOriginString TUGame      = "TU"
toOriginString YouTube     = "YT"
toOriginString TwitCasting = "TC"

-- | convert origin string to its Enum value
fromOriginString :: String -> Origin
fromOriginString "NN" = NicoNico
fromOriginString "TU" = TUGame
fromOriginString "YT" = YouTube
fromOriginString "TC" = TwitCasting
fromOriginString x    = error $ "Unrecognized string found: " ++ x

-- assume the argument is formatted as hh:mm:ss and in 24h format
fromTimeString :: String -> IO UTCTime
fromTimeString str = do
  currTime <- getCurrentTime
  let day = utctDay currTime
  return $ UTCTime day dayTime
  where [hhStr,mmStr,ssStr] = unfoldr (\x -> if null x then Nothing else Just . fmap (drop 1) $ break (== ':') x) str
        hh = (read hhStr - 9 + 24) `mod` 24:: Int
        mm = read mmStr :: Int
        ss = read ssStr :: Int
        dayTime = toEnum $ hh * 3600 + mm * 60 + ss
