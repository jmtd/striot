{-
    demonstration of generating examples/merge via CompileIoT
 -}

import Striot.CompileIoT
import Striot.StreamGraph
import Algebra.Graph
import Language.Haskell.TH
import Control.Concurrent
import System.Random

opts = defaultOpts { imports  = imports defaultOpts ++ [ "System.Random"
                                                       , "GHC.Conc.IO"
                                                       , "System.IO"
                                                       , "Text.Read"
                                                       , "Data.Foldable"
                                                       ]
                   , packages = ["random"]
                   , rewrite  = False
                   }

source = [| do
    i <- getStdRandom (randomR (1,10)) :: IO Int
    let s = show i in do
        threadDelay 1000000
        putStrLn $ "client sending " ++ s
        return s
    |]

ssi =
 [ (Source , [source], "String")
 , (Map    , [[| id |]], "String")
 , (Filter , [[| \i -> (read i :: Int) > 5 |]], "String")
 , (Window , [[| chop 1 |]], "[String]")
 , (Sink   , [[| mapM_ $ putStrLn . ("receiving "++) . show . value |]], "[String]")
 ]

graph = simpleStream ssi

parts = [[1,2],[3,4,5]]

main = do
    partitionGraph graph parts opts
