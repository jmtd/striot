-- node3
import Network
import Striot.FunctionalIoTtypes
import Striot.FunctionalProcessing
import Striot.Nodes
import Control.Concurrent


sink1 :: Show a => [a] -> IO ()
sink1 = mapM_ (putStrLn . show)

streamGraphFn :: Stream String -> Stream [String]
streamGraphFn n0 = let
    n1 = streamMap (\st->"Incoming Message at Server: " ++ st) n0
    n2 = streamWindow ((chop 2)) n1
    in n2


main :: IO ()
main = nodeSink streamGraphFn sink1 9001