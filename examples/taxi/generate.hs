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
import Striot.Jackson hiding (serviceTime)

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
    [ ((Source 1.2),    [source],                         "Trip", 0)
    , (Map,       [[| tripToJourney |]],            "Journey", 0.0001)
    , ((Filter 0.95),    [[| \j -> inRangeQ1 (start j) && inRangeQ1 (end j) |]],"Journey", 0.0001)
    , (Window,    [[| slidingTime 1800000 |]],      "[Journey]", 0.0001)
    , (Map,       [topk'],                          "((UTCTime,UTCTime),[(Journey,Int)])", 0.01)
    , ((FilterAcc 0.1), filterDupes,                      "((UTCTime,UTCTime),[(Journey,Int)])", 0.0001)
    , (Sink,      [sink],                           "((UTCTime,UTCTime),[(Journey,Int)])", 0.0001)
    ]

parts = [[1..4],[5],[6..7]]

main = partitionGraph taxiQ1 parts opts

------------------------------------------------------------------------------
-- jackson stuff

taxiQ1Calc = calcAllSg taxiQ1

------------------------------------------------------------------------------
-- some cost model experiments

costModelStuff = let
    allOpts = allOptimisations taxiQ1

    -- are there any graphs which breach utilisation?
    anyBreach = map and $ concatMap snd $ map (\(x,y) -> (x, map ((>=1).util) y)) $ allOpts
