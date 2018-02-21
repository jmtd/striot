-- node2
import Network
import Striot.FunctionalIoTtypes
import Striot.FunctionalProcessing
import Striot.Nodes
import Control.Concurrent



streamGraphFn :: Stream String -> Stream String
streamGraphFn n0 = let
    n1 = streamMap (\st->reverse st) n0
    in n1


main :: IO ()
main = nodeLink streamGraphFn 9001 "node3" 9001