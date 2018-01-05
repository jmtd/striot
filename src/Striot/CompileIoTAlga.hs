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
                             , StreamOperator      -- used by Optimizer
                          -- , Partition           -- used by Optimizer
                          -- , Id
                          -- , createPartitionsAndEdges
                          -- , graphEdgesWithTypes -- used by VizGraph
                          -- , printParams         -- used by VizGraph
                          -- , generateCode
                             , htf_thisModulesTests
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
    { vertexId :: Int
    , operator :: StreamOperator a
    } deriving (Eq)

instance Ord a => Ord (StreamVertex a) where
    compare (StreamVertex x _) (StreamVertex y _) = compare x y

instance Show a => Show (StreamVertex a) where
    show v = (show (vertexId v))++":"++(show (operator v))

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
s0 = connect (Vertex (StreamVertex 0 (Source "?"))) (Vertex (StreamVertex 1 (Sink "!")))

-- Source -> Filter -> Sink
s1 = path [StreamVertex 0 (Source "?"), StreamVertex 1 Filter, StreamVertex 2 (Sink "!")]

test_reform_s0 = assertEqual s0 (unPartition $ createPartitions s0 [[0],[1]])
test_reform_s1 = assertEqual s1 (unPartition $ createPartitions s1 [[0,1],[2]])
test_reform_s1_2 = assertEqual s1 (unPartition $ createPartitions s1 [[0],[1,2]])
