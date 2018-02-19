module Framework where

import Network.Socket
import Network.WebSocket

hostAddress :: HostAddress
hostAddress = tupleToHostAddress (160, 16, 82, 222)

gamePort :: PortNumber
gamePort = 8888

chatPort :: PortNumber
chatPort = 8891

run :: IO ()
run = do
  conn <- someConnection :: Connection

  forever $ do
    msg <- WS.receiveData conn
    if not $ null _ then _ else return ()
