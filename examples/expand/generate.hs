import Striot.CompileIoT
import Striot.StreamGraph

import Algebra.Graph
import Control.Monad (replicateM)
import System.Random

source = [| do
    indices <- replicateM 10 (getStdRandom (randomR (0,39)) :: IO Int)
    let
        -- 40 words picked randomly from the dictionary, 20 of which are prefixed
        -- with # to simulate hashtags
        randomWords = words
            "Angelica #Seine #sharpened sleeve consonance diabolically\
            \ #bedlam #sharpener sentimentalizing amperage #quilt Ahmed #quadriceps Mia\
            \ #burglaries constricted julienne #wavier #gnash #blowguns wiping somebodies\
            \ nematode metaphorical Chablis #taproom disrespects #oddly ideograph rotunda\
            \ #verdigrised #blazoned #murmuring #clover #saguaro #sideswipe faulted brought\
            \ #Selkirk #Kshatriya"

        s = unwords $ map (randomWords !!) indices in do
        threadDelay 1000000
        putStrLn $ "sending " ++ (show s)
        return s
    |]

v1 = StreamVertex 1 Source [source]                              "String" "String"
v2 = StreamVertex 2 Map    [[| filter (('#'==).head) . words |]] "String" "[String]"

v5 = StreamVertex 5 Expand []                                    "[String]" "String"
v6 = StreamVertex 6 Sink   [[| mapM_ print |]]                   "String" "String"

graph = path [v1, v2, v5, v6]

parts = [[1,2],[5,6]]

main = partitionGraph graph parts defaultOpts
