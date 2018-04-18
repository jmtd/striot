-- node3
import Network
import Striot.FunctionalIoTtypes
import Striot.FunctionalProcessing
import Striot.Nodes
import Control.Concurrent


sink1 :: Show a => Stream a -> IO ()
sink1 = mapM_ (putStrLn . show)

streamGraphFn :: Stream String -> Stream String -> Stream String
streamGraphFn n1 n2 = let
    n3 = streamMerge ([n1,n2])
    in n3


main :: IO ()
main = nodeSink2 streamGraphFn sink1 9001 9002