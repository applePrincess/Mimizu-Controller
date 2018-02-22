{-|
Module      : Mimizu.Chat
Copyright   : (c) Apple Princess 2018
License     : MIT
Maintainer  : Apple Princess
Stability   : experimental
Portability : portable
-}
module Mimizu.Chat where

import           Data.Time.Clock

-- | The representation of one message
data Chat = Chat { origin  :: Origin  -- ^ The origin of message
                 , time    :: UTCTime   -- ^ The time (Internally in UTC, Externally in JST)
                 , sender  :: String  -- ^ The display name of sender the message sent by
                 , message :: String -- ^ The actual message
                 }

-- | The representation of origin where a message come from
data Origin = NicoNico -- ^ NicoNico live stream. See <http://com.nicovideo.jp/community/co3265652>
            | TUGame   -- ^ TUGame original site. See <http://tk2-217-18218.vs.sakura.ne.jp>
            | YouTube  -- ^ YouTube live stream. See <https://www.youtube.com/user/TsUmezawa>

-- | convert origin to chat websocket acceptable string
toOriginString :: Origin -> String
toOriginString NicoNico = "NN"
toOriginString TUGame   = "TU"
toOriginString YouTube  = "YT"

-- | convert origin string to its Enum value
fromOriginString :: String -> Origin
fromOriginString "NN" = NicoNico
fromOriginString "TU" = TUGame
fromOriginString "YT" = YouTube
fromOriginString x    = error $ "Unrecognized string found: " ++ x
