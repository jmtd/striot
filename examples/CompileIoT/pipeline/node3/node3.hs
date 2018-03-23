-- node3
import Network
import Striot.FunctionalIoTtypes
import Striot.FunctionalProcessing
import Striot.Nodes
import Control.Concurrent


sink1 :: Show a => [a] -> IO ()
sink1 = mapM_ (putStrLn . show)

streamGraphFn :: Stream String -> Stream [String]
streamGraphFn n1 = let
    n2 = streamMap (\st->"Incoming Message at Server: " ++ st) n1
    n3 = streamWindow ((chop 2)) n2
    in n3


main :: IO ()
main = nodeSink streamGraphFn sink1 9001