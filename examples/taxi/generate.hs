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
