module Mimizu.Chat where

import Data.Time.Clock
data Chat = Chat { origin :: Origin
                 , time :: UTCTime
                 , sender :: String
                 , message :: String }

data Origin = NicoNico | TUGame | YouTube

toOriginString :: Origin -> String
toOriginString NicoNico = "NN"
toOriginString TUGame   = "TU"
toOriginString YouTube  = "YT"

fromOriginString :: String -> Origin
fromOriginString "NN" = NicoNico
fromOriginString "TU" = TUGame
fromOriginString "YT" = YouTube
fromOriginString x    = error $ "Unrecognized string found: " ++ x
