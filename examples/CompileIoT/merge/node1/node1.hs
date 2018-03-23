-- node1
import Network
import Striot.FunctionalIoTtypes
import Striot.FunctionalProcessing
import Striot.Nodes
import Control.Concurrent


src1 :: IO String
src1 = do
    threadDelay (1000*1000)
    putStrLn "sending 'foo'"
    return "foo"

streamGraphFn :: Stream String -> Stream String
streamGraphFn n1 = let
    n2 = streamMap (Prelude.id) n1
    in n2


main :: IO ()
main = nodeSource src1 streamGraphFn "node3" 9001