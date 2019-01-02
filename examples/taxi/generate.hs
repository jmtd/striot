{-
    generate.hs for Taxi Q1
-}

import Striot.CompileIoT
import Algebra.Graph
import System.FilePath --(</>)

import VizGraph

imports = ["Striot.FunctionalIoTtypes", "Striot.FunctionalProcessing", "Striot.Nodes", "Taxi"
    , "Data.Time" -- UTCTime(..)..
    , "Data.Maybe" -- fromJust
    , "Data.List.Split" -- splitOn
    , "Control.Concurrent"] -- threadDelay

-- XXX move to a source fn in Taxi and not embedded code
source = "do\n\
\   line <- getLine;\n\
\   return $ stringsToTrip $ splitOn \",\" line"

taxiQ1 :: StreamGraph
taxiQ1 = path
    [ StreamVertex 1 Source [source]                              "Trip"    "Trip"
    , StreamVertex 2 Map    ["tripToJourney", "s"]                "Trip"    "Journey"
    , StreamVertex 3 Filter ["(\\j -> inRangeQ1 (start j))", "s"] "Journey" "Journey"
    , StreamVertex 4 Filter ["(\\j -> inRangeQ1 (end j))", "s"]   "Journey" "Journey"
    , StreamVertex 5 Window ["(slidingTime 1800000)", "s"]        "Journey" "[Journey]"

    , StreamVertex 6 Map    ["(\\w -> (let lj = last w in (pickupTime lj, dropoffTime lj), topk 10 w))", "s"] "[Journey]" "((UTCTime,UTCTime),[(Journey,Int)])"
    -- journeyChanges
    , StreamVertex 7 FilterAcc [ "(\\acc h -> if snd h == snd acc then acc else h)"
                               , "(fromJust (value (head s)))"
                               , "(\\h acc -> snd h /= snd acc)"
                               , "(tail s)"
                               ] "((UTCTime,UTCTime),[(Journey,Int)])" "((UTCTime,UTCTime),[(Journey,Int)])"
    , StreamVertex 8 Sink   ["mapM_ print"] "((UTCTime,UTCTime),[(Journey,Int)])" "IO ()"
                            -- XXX putStr$ concatMap (\res->(show $ value res) ++ "\n")
    ]

parts = [[1..5],[6..8]]
partEx = generateCode taxiQ1 parts imports

writePart :: (Char, String) -> IO ()
writePart (x,y) = let
    bn = "node" ++ (x:[])
    fn = bn </> bn ++ ".hs"
    in
        writeFile fn y

main = mapM_ writePart (zip ['1'..] partEx)

------------------------------------------------------------------------------

v0 = StreamVertex 0 Source [source]     "Trip"
-- processedStream
v1 = StreamVertex 1 Map ["s"]       "Trip"
v2 = StreamVertex 2 Filter ["s"]    "Trip"

-- due to representation limitations, we need to duplicate this entire initial
-- bit at the moment
v0b = StreamVertex 100 Source [source]  "Trip"
v1b = StreamVertex 101 Map ["…"] "Trip"
v2b = StreamVertex 102 Filter ["…"] "Trip"

-- streamJoinW
v3a = StreamVertex 3 Window ["…"] "?"
v3b = StreamVertex 33 Window ["…"] "?"
v4 = StreamVertex 4 Join ["…"] "?"
v5 = StreamVertex 5 Map ["…"] "?"

-- streamWindowAggregate
v6 = StreamVertex 6 Window ["…"] "?"
v7 = StreamVertex 7 Map ["…"] "?"

v8 = StreamVertex 8 FilterAcc ["…"] "?"
v9 = StreamVertex 9 Sink ["…"] "?"

-- XXX this will not correctly construct the second set of edges...
-- but do we even need that? just refer to 's' twice in the params to Join!
-- ... streamJoinW applies distinct windows to each stream prior to joining alas
taxiQ2 = path [v0,v1,v2,v3a]                     -- source branch 1
    `Overlay` path [v0b,v1b,v2b,v3b]             -- source branch 2
    `Overlay` ((Vertex v3a `Overlay` Vertex v3b) `Connect` Vertex v4) -- streamJoin
    `Overlay` path [v4,v5,v6,v7,v8,v9]           -- trunk to sink
