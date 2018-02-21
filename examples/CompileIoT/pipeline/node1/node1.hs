-- node1
import Network
import Striot.FunctionalIoTtypes
import Striot.FunctionalProcessing
import Striot.Nodes
import Control.Concurrent


src1 :: IO String
src1 = do
    threadDelay (1000*1000)
    return "Hello from Client!"

streamGraphFn :: Stream String -> Stream String
streamGraphFn n0 = let
    n1 = streamMap (\st->st++st) n0
    in n1


main :: IO ()
main = nodeSource src1 streamGraphFn "node2" 9001