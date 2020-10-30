{-
    demonstration of generating examples/merge via CompileIoT
 -}

module Merge where

import Striot.CompileIoT
import Striot.StreamGraph
import Algebra.Graph
import Language.Haskell.TH
import Control.Concurrent

import Striot.VizGraph

source x y = do
    threadDelay y
    putStrLn $ "sending "++ x
    return x

foz = source "foz" (1000*1000)
bar = source "bar" (500*1000)
baz = source "baz" (200*1000)

beginsWithB = ('b'==) . head
endsWithZ   = ('z'==) . last

v1 = StreamVertex 1 (Source 1) [[| foz |]] "String" "String" 0
v2 = StreamVertex 2 (Source 1) [[| bar |]] "String" "String" 0
v3 = StreamVertex 3 (Source 1) [[| baz |]] "String" "String" 0

v4 = StreamVertex 4 Merge [] "String" "String" 1

v5 = StreamVertex 5 Map [[| reverse |]] "String" "String" 1
v6 = StreamVertex 6 Map [[| map toUpper |]] "String" "String" 1

v6' = StreamVertex 6 Map [[| map toUpper . reverse |]] "String" "String" 2

-- XXX: ^ we lie about the input type here, because the generated function has split-out arguments
v7 = StreamVertex 7 Sink [[| mapM_ print |]] "String" "IO ()" 0

graph = (overlays (map vertex [v1,v2,v3]) `connect` (vertex v4)) `overlay` path [v4,v5,v6,v7]

graph'= (overlays (map vertex [v1,v2,v3]) `connect` (vertex v4)) `overlay` path [v4,v6',v7]

parts = [[1],[2],[3],[4,5,6,7]]

main = partitionGraph graph parts (defaultOpts { imports = "Merge" : imports defaultOpts , rewrite = False})
