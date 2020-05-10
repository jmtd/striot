{-
    demonstration of generating examples/merge via CompileIoT
 -}

import Striot.CompileIoT
import Striot.StreamGraph
import Algebra.Graph
import Language.Haskell.TH

opts = GenerateOpts { imports   = ["Striot.FunctionalIoTtypes", "Striot.FunctionalProcessing", "Striot.Nodes", "Control.Concurrent", "System.Random"]
                    , packages  = ["random"]
                    , preSource = Nothing
                    , rewrite = False
                    }

source = "do\n\
\    i <- getStdRandom (randomR (1,10)) :: IO Int\n\
\    let s = show i in do\n\
\        threadDelay 1000000\n\
\        putStrLn $ \"client sending \" ++ s\n\
\        return s"

exp' = LitE (IntegerL 5)

graph = simpleStream
 [ (Source , [exp'], "String") -- source
 , (Map    , [exp'], "String") --["id", "s"]
 , (Filter , [exp'], "String") --["(\\i -> (read i :: Int) > 5)", "s"]
 , (Window , [exp'], "String") --["(chop 1)", "s"]
 , (Sink   , [exp'], "[String]")  --["mapM_ $ putStrLn . (\"receiving \"++) . show . value"]
 ]

parts = [[1,2],[3,4,5]]

main = partitionGraph graph parts opts
