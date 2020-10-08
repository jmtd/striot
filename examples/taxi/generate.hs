{-# LANGUAGE TemplateHaskell #-}
{-
    generate.hs for Taxi Q1
-}

import Striot.CompileIoT
import Striot.StreamGraph
import Algebra.Graph
import Data.Time -- UTCTime(..)
import Data.Maybe (fromJust)
import Data.List.Split (splitOn)

import Data.Array -- cabal install array
import Striot.Jackson

import Striot.VizGraph

opts = GenerateOpts { imports = imports defaultOpts ++
                        [ "Taxi"
                        , "Data.Maybe"
                        , "Data.List.Split"
                        , "Data.Time" -- UTCTime(..)..
                        ]
                    , packages = []
                    , preSource = Just "preSource"
                    , rewrite = False
                    }
source = [| getLine >>= return . stringsToTrip . splitOn "," |]

topk' = [| \w -> (let lj = last w in (pickupTime lj, dropoffTime lj), topk 10 w) |]

filterDupes = [ [| \_ h -> Just h |]
              , [| Nothing |]
              , [| \h wacc -> case wacc of Nothing -> True; Just acc -> snd h /= snd acc |]
              ]

sink = [| mapM_ (print.show.fromJust.value) |]

taxiQ1 :: StreamGraph
taxiQ1 = simpleStream
    [ (Source,    [source],                         "Trip", 0)
    , (Map,       [[| tripToJourney |]],            "Journey", 1)
    , ((Filter 0.95),    [[| \j -> inRangeQ1 (start j) && inRangeQ1 (end j) |]],"Journey", 1)
    , (Window,    [[| slidingTime 1800000 |]],      "[Journey]", 1)
    , (Map,       [topk'],                          "((UTCTime,UTCTime),[(Journey,Int)])", 1)
    , ((FilterAcc 0.1), filterDupes,                      "((UTCTime,UTCTime),[(Journey,Int)])", 1)
    , (Sink,      [sink],                           "((UTCTime,UTCTime),[(Journey,Int)])", 0)
    ]

parts = [[1..4],[5],[6..7]]

main = partitionGraph taxiQ1 parts opts

------------------------------------------------------------------------------
-- jackson stuff

-- data needed. Could be folded into the StreamVertex or PartitionMap types.
taxiQ1arrivalRate = 1.2 -- arrival rate into the system
-- distribution of arriving events across source nodes
taxiQ1inputDistribution = [ (1, 1.0) ] :: [(Int, Double)]

taxiQ1meanServiceTimes:: Array Int Double
taxiQ1meanServiceTimes = listArray (1,6) [0.0001,0.0001,0.0001,0.01,0.0001,0.0001]

-- derived (could be hidden)
taxiQ1Array = calcPropagationArray taxiQ1
taxiQ1Inputs = calcInputs taxiQ1

taxiQ1Calc:: [OperatorInfo]
taxiQ1Calc = calcAll taxiQ1Array taxiQ1Inputs taxiQ1arrivalRate taxiQ1meanServiceTimes

-- wrapping the above up for convenience
taxiParams = JacksonParams taxiQ1arrivalRate taxiQ1inputDistribution

------------------------------------------------------------------------------
-- applying the above

allThreeNodePartitionings = filter ((==3) . length) (allPartitions taxiQ1)
allThreeNodeSubGraphs = map (\pm -> createPartitions taxiQ1 pm) allThreeNodePartitionings
-- partitions are backwards, i.e. [[7],[6],[5,4,3,2,1]], does that matter?
-- Doesn't seem to
