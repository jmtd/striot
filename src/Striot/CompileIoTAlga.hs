{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-
    attempt to recast CompileIoT in terms of Alga
    Alga has a parameterized type Graph a, so we need to figure out what a
    should be such that vertices are uniquely identified
 -}

module Striot.CompileIoTAlga ( createPartitions    -- used by Optimizer
                          -- , StreamGraph(..)     -- used by Optimizer
                          -- , StreamOperation(..) -- used by Optimizer
                             , StreamVertex
                             , vertexId, operator
                             , StreamOperator      -- used by Optimizer
                          -- , Partition           -- used by Optimizer
                          -- , Id
                          -- , createPartitionsAndEdges
                          -- , graphEdgesWithTypes -- used by VizGraph
                          -- , printParams         -- used by VizGraph
                             , generateCode
                             , PartitionMap -- implementation feature of this module
                             , htf_thisModulesTests
                             , s0, s1, s2
                             ) where

import Algebra.Graph
import Test.Framework

-- decorating with comments corresponding to valence/def in the draft paper

data StreamOperator a = Map       -- (a -> b)      -> Stream a           -> Stream b
                      | Filter    -- (a -> Bool)   -> Stream a           -> Stream a
                      | Expand    -- Stream [a]    -> Stream a
                      | Window    -- (Stream a     -> Stream [a])        -> Stream a      -> Stream [a]
                      | Merge     -- [Stream a]    -> Stream a
                      | Join      -- Stream a      -> Stream b           -> Stream (a,b)
                      | Scan      -- (b -> a -> b) -> b                  -> Stream a      -> Stream b                  -- "stream map with accumulating parameter"
                      | FilterAcc -- (b -> a -> b) -> b (a -> b -> Bool)                                                                 -> Stream a -> Stream a
                      | Source a
                      | Sink a deriving (Show,Eq)

-- Id needed for uniquely identifying a vertex. (Is there a nicer way?)
data StreamVertex a = StreamVertex
    { vertexId   :: Int
    , operator   :: StreamOperator a
    , parameters :: [String] -- XXX strings of code. From CompileIoT. Variable length e.g.FilterAcc takes 3 (?)
    -- this is conflated with StreamOperator's parameter above; I'd prefer to add more params there.
    } deriving (Eq)

instance Ord a => Ord (StreamVertex a) where
    compare (StreamVertex x _ _) (StreamVertex y _ _) = compare x y

instance Show a => Show (StreamVertex a) where
    show v = (map (\x->if x=='"' then '\'' else x) (show (operator v))) ++
        (concatMap (\s->"("++s++") ") (parameters v))
    -- XXX: temporary translate " to ' so output is "dot syntax clean"

-- XXX: add a type definition for Graph (StreamVertex a)... and/or concrete instead of a?
-- …for exporting

------------------------------------------------------------------------------
-- StreamGraph Partitioning

type PartitionMap = [[Int]]
-- outer-list index: partition ID
-- each inner-list is a list of Vertex IDs to include in that partition

-- createPartitions returns ([partition map], [inter-graph links])
-- where inter-graph links are the cut edges due to partitioning
createPartitions :: Ord a => Graph (StreamVertex a) -> PartitionMap -> ([Graph (StreamVertex a)], [Graph (StreamVertex a)])
createPartitions _ [] = ([],[])
createPartitions g (p:ps) = ((overlay vs es):tailParts, cutEdges ++ tailCuts) where
    vs        = vertices $ filter fv (vertexList g)
    es        = edges $ filter (\(v1,v2) -> (fv v1) && (fv v2)) (edgeList g)
    cutEdges  = if edgesOut == empty then [] else [edgesOut]
    fv v      = (vertexId v) `elem` p
    edgesOut  = edges $ filter (\(v1,v2) -> (fv v1) && (not(fv v2))) (edgeList g)
    (tailParts, tailCuts) = createPartitions g ps

unPartition :: Ord a => ([Graph (StreamVertex a)], [Graph (StreamVertex a)]) -> Graph (StreamVertex a)
unPartition (a,b) = foldl overlay Empty (a ++ b)

------------------------------------------------------------------------------
-- tests / test data

-- Source -> Sink
s0 = connect (Vertex (StreamVertex 0 (Source "?") [])) (Vertex (StreamVertex 1 (Sink "!") []))

-- Source -> Filter -> Sink
s1 = path [StreamVertex 0 (Source "?") [], StreamVertex 1 Filter [], StreamVertex 2 (Sink "!") []]

-- port of s1 from CompileIoT
s2 = path [ StreamVertex 0 (Source "sourceGen") ["sourceGen"]--                                                                           "Stream Trip"            ["Taxi.hs","SourceGenerator.hs"],
          , StreamVertex 1 Map                  ["\\t-> Journey{start=toCellQ1 (pickup t), end=toCellQ1 (dropoff t)}"]--                  "Stream Journey"         ["Taxi.hs"],
          , StreamVertex 2 Filter               ["\\j-> inRangeQ1 (start j) && inRangeQ1 (end j)"]--                                      "Stream Journey"         ["Taxi.hs"],
          , StreamVertex 3 Window               ["slidingTime 1800"] --                                                                   "Stream [Journey]"       ["Taxi.hs"],
          -- StreamOperation opid [opinputs] operator [parameters: "mostFrequent 10"] outputtype:"Stream [(Journey,Int)]" imports:["Taxi.hs"],
          , StreamVertex 4 Map                  ["mostFrequent 10"] --                                                                    "Stream [(Journey,Int)]" ["Taxi.hs"],
          , StreamVertex 5 FilterAcc            ["value $ head s","\\h acc-> if (h==acc) then acc else h","\\h acc->(h/=acc)","tail s"]-- "Stream [(Journey,Int)]" ["Taxi.hs"],
          , StreamVertex 6 (Sink "print")       ["print"] --                                                                              ""                                []]
          ]

test_reform_s0 = assertEqual s0 (unPartition $ createPartitions s0 [[0],[1]])
test_reform_s1 = assertEqual s1 (unPartition $ createPartitions s1 [[0,1],[2]])
test_reform_s1_2 = assertEqual s1 (unPartition $ createPartitions s1 [[0],[1,2]])

type StreamGraph = Graph (StreamGraph String)
generateCode:: StreamGraph -> PartitionMap -> [String] {-stdImports-} -> [String]
