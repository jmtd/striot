{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}

-- demonstrate detecting over-utilisation


module OverUt where

import Striot.CompileIoT
import Striot.StreamGraph
import Striot.LogicalOptimiser
import Striot.VizGraph
import Algebra.Graph
import System.Random

import Striot.Jackson hiding (serviceTime)

import Data.Function ((&))
import Test.Framework hiding ((===))
import Data.Maybe

source = [| do
    i <- getStdRandom (randomR (1,10)) :: IO Int
    threadDelay 1000000
    putStrLn $ "client sending " ++ (show i)
    return i
    |]

merge = StreamVertex 4 Merge [] "Int" "Int" (1/5)

subgraph1 = path
    [ StreamVertex 0 (Source 8)     [ source         ] "Int" "Int" 0
    , StreamVertex 1 (Filter (1/2)) [[|(>5)        |]] "Int" "Int" 0
    , merge
    , StreamVertex 5 Sink           [[|mapM_ print |]] "Int" "Int" 0
    ]

subgraph2 = path
    [ StreamVertex 2 (Source 8)     [   source ] "Int" "Int" 0
    , StreamVertex 3 (Filter (1/2)) [[| (>5) |]] "Int" "Int" 0
    , merge
    ]

graph = overlay subgraph1 subgraph2
parts = [[0,1],[2,3],[4,5]]
main  = partitionGraph graph parts defaultOpts { imports = "System.Random" : imports defaultOpts }

-- filterMerge is not matching on this graph. Why?
test_match = assertBool $ isJust $ firstMatch graph filterMerge

f  = fromJust $ firstMatch graph filterMerge
g' = f graph -- rewritten hosted filter
moo = calcAllSg g'
-- these calcuations are broken probably because there's >1 source node. Let's try another

f'  = fromJust $ firstMatch subgraph1 filterMerge
g'' = f' subgraph1
moo'= calcAllSg g''

-- for some reason I'm getting [1,2,3]
test_opIds = assertEqual [1,4,5] $ map opId moo'

isOverUtilised :: StreamGraph -> Bool
isOverUtilised g = 
    g & calcAllSg & map util & map (>1) & or

test_isOverUtilised = assertBool $ isOverUtilised g''
