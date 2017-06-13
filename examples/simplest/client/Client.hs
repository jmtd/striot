--import Network
import Control.Concurrent
import System.IO
import FunctionalProcessing
import FunctionalIoTtypes
import Nodes
import Network

-- processes source before sending it to another node
main :: IO ()
main = do
         threadDelay (1 * 1000 * 1000)
         nodeSource src streamGraph ("haskellserver"::HostName) (9001::PortNumber)

streamGraph :: Stream String -> Stream String
streamGraph s = streamMap id s where
    id = Prelude.id

src :: IO String
src = clockStreamNamed "Hello from Client!" 1000

clockStreamNamed :: String -> Int -> IO String -- returns the (next) payload to be added into an event and sent to a server
clockStreamNamed message period = do -- period is in ms
                                    threadDelay (period*1000)
                                    return message
