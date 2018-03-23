-- node2
import Network
import Striot.FunctionalIoTtypes
import Striot.FunctionalProcessing
import Striot.Nodes
import Control.Concurrent



streamGraphFn :: Stream String -> Stream String
streamGraphFn n1 = let
    n2 = streamMap (\st->reverse st) n1
    in n2


main :: IO ()
main = nodeLink streamGraphFn 9001 "node3" 9001