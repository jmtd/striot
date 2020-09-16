{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}

module Striot.Jackson ( OperatorInfo(..)
                      , calcAll

                      , calcPropagationArray
                      , calcInputs
                      , JacksonParams(..)

                      ) where

-- import FunctionalIoTtypes
-- import FunctionalProcessing
import Data.Array -- cabal install array
import Matrix.LU -- cabal install dsp
import Matrix.Matrix
import Data.List
import Test.Framework
import Data.Maybe (fromMaybe)

import Striot.StreamGraph
import Striot.CompileIoT

import Algebra.Graph

-- References & Manuals
-- https://en.wikipedia.org/wiki/Jackson_network
-- http://www.ece.virginia.edu/mv/edu/715/lectures/QNet.pdf
-- https://hackage.haskell.org/package/dsp 
-- http://haskelldsp.sourceforge.net/doc/Matrix.LU.html
-- https://hackage.haskell.org/package/array-0.5.1.1/docs/Data-Array.html
-- http://haskelldsp.sourceforge.net/doc/Matrix.Matrix.html

-- |Derive the identity matrix from a 2D Array
identity:: (Ix a, Integral a, Num b) => Array (a,a) b -> Array (a,a) b
identity p = listArray (bounds p) $ [if row==column then 1 else 0| row<-[1..size],column<-[1..size]]
                        where size = fst $ snd $ bounds p
                        
-- |Matrix subtraction.
-- The indexes must begin at 1.
mm_subtract:: (Ix a, Integral a, Num b) => Array (a, a) b -> Array (a, a) b -> Array (a, a) b
mm_subtract x y = listArray (bounds x) $ [(x Data.Array.! (row,column))-(y Data.Array.! (row,column))| row<-[1..size],column<-[1..size]] 
                          where size = fst $ snd $ bounds x
                          
-- | Matrix multiplication.
-- The indexes must begin at 1.
ma_mult:: (Ix a, Integral a, Num b) => Array (a, a) b -> b -> Array (a, a) b 
ma_mult x v   = listArray (bounds x) $ [v*(x Data.Array.! (row,column))| row<-[1..size],column<-[1..size]] 
                          where size = fst $ snd $ bounds x
                          
-- | Vector (1D Array) multiplication by value.
-- The indexes must begin at 1.
va_mult:: (Ix a, Integral a, Num b) => Array a b -> b -> Array a b 
va_mult x val   = listArray (bounds x) $ [val*(x Data.Array.! row)| row<-[1..size]] 
                          where size = snd $ bounds x
                          
-- | Vector (1D Array) multiplication.
-- The indexes must begin at 1.
vv_mult:: (Ix a, Integral a, Num b) => Array a b -> Array a b -> Array a b
vv_mult v1 v2 = listArray (bounds v1) $ [(v1 Data.Array.! row)*(v2 Data.Array.!row) |row <- [1..size]]
                          where size = snd $ bounds v1

-- | Vector (1D Array) equivalent of `take`
v_take:: Int -> Array Int b -> Array Int b
v_take max v = listArray (1,max) $ [v Data.Array.! row |row <- [1..max]]

-- Jackson Network: lambda = (I-P')^(-1)a where a = (alpha.p0i)i=1..J
arrivalRate:: Array (Int, Int) Double -> Array Int Double -> Double -> Array Int Double  
-- p - selectivities of filters
-- p0i - distribution of input events into the system (i.e. to which nodes, which are the source nodes)
-- alpha- arrival rate into the system
arrivalRate p p0i alpha =  mv_mult (inverse $ mm_subtract (identity p) (m_trans p)) aa
                               where aa = va_mult p0i alpha
  
-- ρ = λ/μ is the utilization of the buffer (the average proportion of time which the server is occupied.  
utilisation:: Array Int Double -> Array Int Double -> Array Int Double
utilisation arrivalRates meanServiceTimes = vv_mult arrivalRates meanServiceTimes

-- the average number of customers in the system is ρ/(1 − ρ)
avgeNumberOfCustomersInSystem:: Array Int Double -> Array Int Double
avgeNumberOfCustomersInSystem utilisations = listArray (bounds utilisations) $ 
                                                       [(utilisations Data.Array.! row)/(1.0- (utilisations Data.Array.!row)) |row <- [1..size]]
                                                 where size = snd $ bounds utilisations
                                                 
-- the average response time (total time a customer spends in the system) is 1/(μ − λ)
avgeResponseTime:: Array Int Double -> Array Int Double -> Array Int Double
avgeResponseTime arrivalRates meanServiceTimes = listArray (bounds arrivalRates) $ 
                                                       [1.0/((1.0/(meanServiceTimes Data.Array.! row))-(arrivalRates Data.Array.!row)) |row <- [1..size]]
                                                 where size = snd $ bounds arrivalRates

stable:: Array Int Double -> Array Int Double -> Array Int Bool
stable arrivalRates meanServiceTimes = let utils = utilisation arrivalRates meanServiceTimes in
                                           listArray (bounds arrivalRates) $ 
                                                       [(utils Data.Array.! row) < 1/0 |row <- [1..size]]
                                                 where size = snd $ bounds arrivalRates

--	the average time spent waiting in the queue is ρ/(μ – λ)
avgeTimeInQueue:: Array Int Double -> Array Int Double -> Array Int Double
avgeTimeInQueue arrivalRates meanServiceTimes = let utils = utilisation arrivalRates meanServiceTimes in
                                                       listArray (bounds arrivalRates) $ 
                                                       [(utils Data.Array.! row)/
                                                        ((1.0/(meanServiceTimes Data.Array.! row))-(arrivalRates Data.Array.!row))  |row <- [1..size]]
                                                    where size = snd $ bounds arrivalRates


------ example from wikipedia page on Jackson networks
wikiExample:: Array Int Double
wikiExample = let p     = listArray ((1,1),(3,3)) $ [0  ,0.5,0.5,     -- node 1
                                                     0  ,0  ,0  ,     -- node 2
                                                     0  ,0  ,0   ] in -- node 3
              let alpha = 5 in                                        -- 5 events per second arrive into the system
              let p0i   = listArray (1,3) $         [0.5,0.5,0   ] in -- the input events are distributed evenly across nodes 1 and 2
                  arrivalRate p p0i alpha

--- Taxi Q1 example
{--
type Q1Output = ((UTCTime, UTCTime), [(Journey, Int)])
frequentRoutes :: Stream Trip -> Stream Q1Output                                                          -- node 6  Input rate 1.188*0.1 = 0.1188 
frequentRoutes s = streamFilterAcc (\_ h -> (False,h)) (True,undefined) testSndChange s                   -- node 5  Input rate 1.188 Selectivity (est. 0.1)
                 $ streamMap (\w -> (let lj = last w in (pickupTime lj, dropoffTime lj), topk 10 w))      -- node 4  Input rate 1.188
                 $ streamWindow (slidingTime 1800000)                                                     -- node 3  Input rate 1.2*0.99 = 1.188
                 $ streamFilter (\j -> inRangeQ1 (start j) && inRangeQ1 (end j))                          -- node 2  Input rate 1.2/s Selectivity (est.) 0.95
                 $ streamMap    tripToJourney s                                                           -- node 1  Input rate 1.2/s         

Node, InputFrom, Input Rate, Selectivity, Output Rate
0     -          -           -            1.2
1     0 (1)      1.2         1            1.2
2     1 (1)      1.2         0.95         1.188
3     2 (0.95)   1.188       1            1.188
4     3 (1)      1.188       1            1.188
5     4 (1)      1.188       0.1          0.1188
6     5 (0.1)    0.118       -            - 

This is represented as follows:      
-}
       
taxiQ1Array:: Array (Int,Int) Double
taxiQ1Array = listArray ((1,1),(6,6)) $
              -- Output Node
              --  1    2    3    4    5     6
               [  0   ,1   ,0   ,0   ,0    ,0  ,     -- node 1 streamMap
                  0   ,0   ,0.95,0   ,0    ,0  ,     -- node 2 streamFilter
                  0   ,0   ,0   ,1   ,0    ,0  ,     -- node 3 streamWindow
                  0   ,0   ,0   ,0   ,1    ,0  ,     -- node 4 streamMap
                  0   ,0   ,0   ,0   ,0    ,0.1,     -- node 5 streamFilterAcc
                  0   ,0   ,0   ,0   ,0    ,0  ]     -- node 6 the output of Q1

-- this is a routing table, effectively.
--
-- the probability of an event leaving node2 (filter) and arriving at node3 (window) is 0.95, i.e. this is the filter selectivity
 
taxiQ1Inputs = listArray (1,6) $ [1,0,0,0,0,0] -- all events in the input stream are sent to node 1

-- 6 nodes. is that input and 5 operators, or 5 operators and output?
-- the longest service time is window in the former case and map in the latter
-- I'm guessing it's window and this is modelling the buffering time
taxiQ1meanServiceTimes:: Array Int Double
taxiQ1meanServiceTimes = listArray (1,6) [0.0001,0.0001,0.0001,0.9,0.0001,0.0001]

 
taxiQ1arrivalRates:: Array Int Double
taxiQ1arrivalRates = arrivalRate taxiQ1Array taxiQ1Inputs 1.2 -- the 1.2 is the arrival rate (in events per second) into the system



taxiQ1utilisation = utilisation taxiQ1arrivalRates taxiQ1meanServiceTimes 

taxiQ1avgeNumberCustomersInSystem = avgeNumberOfCustomersInSystem taxiQ1utilisation

taxiQ1avgeResponseTime = avgeResponseTime taxiQ1arrivalRates taxiQ1meanServiceTimes

taxiQ1avgeTimeInQueue = avgeTimeInQueue   taxiQ1arrivalRates taxiQ1meanServiceTimes

data OperatorInfo = OperatorInfo { opId        :: Int
                                 , arrRate     :: Double
                                 , serviceTime :: Double
                                 , util        :: Double
                                 , stab        :: Bool
                                 , custInSys   :: Double
                                 , respTime    :: Double
                                 , queueTime   :: Double
                                 }
                                 deriving (Show)
                                 
calcAll:: Array (Int,Int) Double -> Array Int Double -> Double -> Array Int Double -> [OperatorInfo]
calcAll connections inputs alpha meanServiceTimes = let
    arrivalRates             = arrivalRate connections inputs alpha
    utilisations             = utilisation arrivalRates meanServiceTimes
    stability                = stable arrivalRates meanServiceTimes
    avgeNumberOfCustInSystem = avgeNumberOfCustomersInSystem utilisations
    avgeResponseTimes        = avgeResponseTime arrivalRates meanServiceTimes
    avgeTimesInQueue         = avgeTimeInQueue  arrivalRates meanServiceTimes

    in map (\id -> OperatorInfo id (arrivalRates             ! id)
                                   (meanServiceTimes         ! id)
                                   (utilisations             ! id)
                                   (stability                ! id)
                                   (avgeNumberOfCustInSystem ! id)
                                   (avgeResponseTimes        ! id)
                                   (avgeTimesInQueue         ! id))
           [1.. (snd $ bounds arrivalRates)]
                              
taxiQ1Calc:: [OperatorInfo]
taxiQ1Calc = calcAll taxiQ1Array taxiQ1Inputs 1.2 taxiQ1meanServiceTimes


-- basic tests
ex1   = listArray ((1,1),(3,3)) $ [1,0,0,-0.5,1,0,-0.5,0,1]                    
test1 = (listArray ((1,1), (3,3)) $ [1,0,0,0,1,0,0,0,1]) Data.Array.! (1,1)
test2 = print $ inverse $ listArray ((1,1), (3,3)) $ [1,0,0,0,1,0,0,0,1]
test3 = print $ inverse $ listArray ((1,1), (3,3)) $ [1,0,0,-0.5,1,0,-0.5,0,1]
test4 = print $ mv_mult (inverse $ listArray ((1,1), (3,3)) $ [1,0,0,-0.5,1,0,-0.5,0,1]) 
                        (listArray (1,3) $ [2.5,2.5,0])
test5 = identity ex1
test6 = mm_subtract ex1 ex1
test7 = m_trans ex1
test8 = mm_subtract (identity ex1) (m_trans ex1)

prop_identity = do
    n <- vectorOf 9 arbitrary :: Gen [Double]
    return $ identity (listArray shape n)
        == listArray shape
           ([1, 0, 0
            ,0, 1, 0
            ,0, 0, 1
            ] :: [Double])
    where shape = ((1,1),(3,3))

------------------------------------------------------------------------------
-- convert a streamgraph into an adjacency matrix

-- copied from examples/taxi/generate.hs, adjusted to match the simplied version
-- above (fused filters; remove the window/expand hack for fixing up timestamps)
taxiQ1 :: StreamGraph
taxiQ1 = simpleStream
    [ (Source,    [source],                         "Trip", 0)
    , (Map,       [[| tripToJourney |]],            "Journey", 1)
    , (Filter,    [[| \j -> inRangeQ1 (start j) && inRangeQ1 (end j) |]],"Journey", 2)
    , (Window,    [[| slidingTime 1800000 |]],      "[Journey]", 3)
    , (Map,       [topk'],                          "((UTCTime,UTCTime),[(Journey,Int)])", 4)
    , (FilterAcc, filterDupes,                      "((UTCTime,UTCTime),[(Journey,Int)])" , 5)
    , (Sink,      [sink],                           "((UTCTime,UTCTime),[(Journey,Int)])", 6)
    ]
sink = [| mapM_ (print.show.fromJust.value) |]
source = [| getLine >>= return . stringsToTrip . splitOn "," |]
topk' = [| \w -> (let lj = last w in (pickupTime lj, dropoffTime lj), topk 10 w) |]
filterDupes = [ [| \_ h -> Just h |]
              , [| Nothing |]
              , [| \h wacc -> case wacc of Nothing -> True; Just acc -> snd h /= snd acc |]
              ]

taxiQ1arrivalRate' = 1.2 -- arrival rate into the system
taxiQ1selectivity = [ (3, 0.95) -- nodeId 2 (filter), selectivity
                    , (6, 0.1) ] :: [(Int, Double)]
-- distribution of arriving events across source nodes
taxiQ1inputDistribution = [ (1, 1.0) ] :: [(Int, Double)]

-- | Calculate the P propagation array for a StreamGraph based on its
-- filter selectivity map.
calcPropagationArray :: StreamGraph -> [(Int, Double)] -> Array (Int, Int) Double
calcPropagationArray g selectivity = let
    vl = vertexList g
    el = map (\(x,y) -> (vertexId x, vertexId y)) (edgeList g)
    look v (f,t) = if   f == vertexId v
                   then fromMaybe 1 $ lookup (vertexId v) selectivity
                   else 0
    m = length vl - 1 -- XXX adjusting for 1 Source node
    in listArray ((1,1),(m,m)) $ concatMap (\v -> map (look v) el) (tail vl)
    --                              XXX adjusting for 1 Source node ^^^^

test_calcPropagationArray = assertEqual taxiQ1Array $
    calcPropagationArray taxiQ1 taxiQ1selectivity

-- | Calculate an Inputs array from a StreamGraph (whether a node is a Source
-- or not)
calcInputs :: StreamGraph -> Array Int Double
calcInputs sg = let
    vl = init (vertexList sg) -- ignore sink node
    n  = length vl
    ms = map (\v -> if operator v == Source then 1 else 0) vl
    in listArray (1,n) $ ms

test_calcInputs = assertEqual taxiQ1Inputs $ calcInputs taxiQ1

-- we need to know:
--  • the arrival rate into the system
--  • the distribution of arriving events amongst source nodes (taxiQ1Inputs), sums to 1
--  • the distribution of inputs (summing to 1): for one source node, just '1' for that node
--    the probability of routing events from one node to another (so, largely similar to an
--    identity matrix, with guaranteed routing between the nodes of the path down the diagonal,
--    but non-1 for filters) — taxiQ1Array
--
--          this can be largely inferred except for filter selectivity from the
--          graph topology. But we need the user to specify the selectivity somehow.
--          if not in the graph types, then is there any point having something other
--          than the whole matrix?

-- let's perhaps focus just on arrivalRates for now
--
--      requires the routing matrix (taxiQ1Array) and the top-level arrivalrate
--      constant 

-- try to produce something that is equal to taxiQ1arrivalRates
-- it's an array of length matching #nodes in graph, numbering from 1, with the
-- initial node arrival rate a constant , and the rest calculated from someting
-- else

taxiQ1arrivalRates' = let
    n = 1
    m = length $ vertexList taxiQ1
    a = 1.2 -- initial arrival rate
    in arrivalRate taxiQ1Array taxiQ1Inputs a

main = htfMain htf_thisModulesTests

-- wrap up all the inputs into a single type
data JacksonParams = JacksonParams { -- arrival rates into the system
                                     jacksonArrivalRate      :: Double
                                   -- distribution of the above across input nodes
                                   , jacksonDistribution     :: [(Int, Double)]
                                   -- filter-type operator selectivities
                                   , jacksonSelectivity      :: [(Int, Double)]
                                   }