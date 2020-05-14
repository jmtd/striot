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

ssi = -- simpleStream input
 [ (Source , [[| source |]], "String")
 , (Map    , [[| id |]], "String")
 , (Filter , [[| \i -> (read i :: Int) > 5 |]], "String")
 , (Window , [[| chop 1 |]], "String")
 , (Sink   , [[| mapM_ $ putStrLn . ("receiving"++) . show . value |]], "[String]")
 ]

graph = simpleStream ssi

parts = [[1,2],[3,4,5]]

runVertex :: StreamVertexQ -> IO StreamGraph
runVertex v@(StreamVertexQ i ops qpars inT outT) = do
    pars <- mapM runQ qpars
    return $ Vertex (StreamVertex i ops pars inT outT)

deQ :: Graph StreamVertexQ -> IO StreamGraph
deQ = foldg
    (return empty)
    runVertex
    (\ x y -> do
        x' <- x
        y' <- y
        return (overlay x' y')
    )
    (\ x y -> do
        x' <- x
        y' <- y
        return (connect x' y')
    )

main = do
    g <- deQ graph
    partitionGraph g parts opts
