{-
    generate.hs for Taxi Q1
-}

import Striot.CompileIoT
import Striot.StreamGraph
import Striot.Jackson
import Algebra.Graph
import Data.Time -- UTCTime(..)
import Data.Maybe (fromJust)
import Data.List.Split (splitOn)

opts = GenerateOpts { imports = imports defaultOpts ++
                        [ "Taxi"
                        , "Data.Maybe"
                        , "Data.List.Split"
                        , "Data.Time" -- UTCTime(..)..
                        ]
                    , packages = []
                    , preSource = Just "preSource"
                    , rewrite = True
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
    [ (Source,    [source],                         "Trip")
    , (Map,       [[| tripToJourney |]],            "Journey")
    , (Filter,    [[| \j -> inRangeQ1 (start j) && inRangeQ1 (end j) |]],"Journey")
    , (Window,    [[| slidingTime 1800000 |]],      "[Journey]")
    , (Map,       [topk'],                          "((UTCTime,UTCTime),[(Journey,Int)])")
    , (FilterAcc, filterDupes,                      "((UTCTime,UTCTime),[(Journey,Int)])")
    , (Sink,      [sink],                           "((UTCTime,UTCTime),[(Journey,Int)])")
    ]

parts = [[1..7],[8],[9..10]]

main = partitionGraph taxiQ1 parts opts

------------------------------------------------------------------------------
-- jackson stuff

-- data needed. Could be folded into the StreamVertex or PartitionMap types.
taxiQ1arrivalRate = 1.2 -- arrival rate into the system
taxiQ1selectivity = [ (3, 0.95) -- nodeId 2 (filter), selectivity
                    , (6, 0.1) ] :: [(Int, Double)]
-- distribution of arriving events across source nodes
taxiQ1inputDistribution = [ (1, 1.0) ] :: [(Int, Double)]

taxiQ1meanServiceTimes:: Array Int Double
taxiQ1meanServiceTimes = listArray (1,6) [0.0001,0.0001,0.0001,0.01,0.0001,0.0001]

-- derived (could be hidden)
taxiQ1Array = calcPropagationArray taxiQ1 taxiQ1selectivity
taxiQ1Inputs = calcInputs taxiQ1

taxiQ1Calc:: [OperatorInfo]
taxiQ1Calc = calcAll taxiQ1Array taxiQ1Inputs taxiQ1arrivalRate taxiQ1meanServiceTimes

-- wrapping the above up for convenience
taxiParams = JacksonParams taxiQ1arrivalRate taxiQ1inputDistribution taxiQ1selectivity
