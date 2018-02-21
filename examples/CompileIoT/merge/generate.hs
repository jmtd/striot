{-
    demonstration of generating examples/merge via CompileIoT
 -}

import Striot.CompileIoTAlga
import Algebra.Graph
import System.FilePath --(</>)

imports = ["Striot.FunctionalIoTtypes", "Striot.FunctionalProcessing", "Striot.Nodes", "Control.Concurrent"]

source x = "do\n\
\    threadDelay (1000*1000)\n\
\    putStrLn \"sending '"++x++"'\"\n\
\    return \"foo\""


--streamMerge:: [Stream alpha]-> Stream alpha
mergeEx :: StreamGraph
mergeEx = path [ StreamVertex 1 Source [source "foo"]              "String"
               , StreamVertex 2 Map    ["id"]                      "String"

               , StreamVertex 3 Source [source "bar"]              "String"
               , StreamVertex 4 Map    ["id"]                      "String"

               , StreamVertex 5 Merge  ["[s1,s2]"]                 "[String]"
               , StreamVertex 6 Window ["(chop 2)"]                "String"
               , StreamVertex 7 Sink   ["mapM_ (putStrLn . show)"] "[String]"
               ]

parts = [[1,2],[3,4],[5,6,7]]
partEx = generateCode mergeEx parts imports

writePart :: (Char, String) -> IO ()
writePart (x,y) = let
    bn = "node" ++ (x:[])
    fn = bn </> bn ++ ".hs"
    in
        writeFile fn y

main = mapM_ writePart (zip ['1'..] partEx)
