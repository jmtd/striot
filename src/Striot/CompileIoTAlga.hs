{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-
    attempt to recast CompileIoT in terms of Alga
    Alga has a parameterized type Graph a, so we need to figure out what a
    should be such that vertices are uniquely identified
 -}

module Striot.CompileIoTAlga ( createPartitions
                             , StreamVertex
                             , StreamOperator
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

-- being careful here to avoid characters that graphViz would not like in an attribute
instance Show a => Show (StreamVertex a) where
    show v = (show (vertexId v))++":"++(show (operator v))

-- Source -> Sink
s0 = connect (Vertex (StreamVertex 0 (Source "?"))) (Vertex (StreamVertex 1 (Sink "!")))

-- Source -> Filter -> Sink
s1 = path [StreamVertex 0 (Source "?"), StreamVertex 1 Filter, StreamVertex 2 (Sink "!")]

------------------------------------------------------------------------------
-- attempt some partitioning

type PartitionMap = [[Int]] -- XXX use Data.Map? (or another Map?)
-- each inner-list is a list of Vertex IDs to include in that partition
-- avoiding explicit partition IDs for now but we might need them later
-- for sub-graph inter-connections


-- XXX: attempt to synthesise additional sub-graphs that encode the edges
-- between partitions.
-- 
-- return is a pair of lists of streamgraphs, fst matches the partition map,
-- snd is the inter-graph links
createPartitions :: Ord a => Graph (StreamVertex a) -> PartitionMap -> ([Graph (StreamVertex a)], [Graph (StreamVertex a)])
createPartitions _ [] = ([],[])
createPartitions g (p:ps) = ((overlay vs es):foo, cutEdges ++ bar) where
    fv        = \v -> (vertexId v) `elem` p
    vs        = vertices $ filter fv (vertexList g)
    es        = edges $ filter (\(v1,v2) -> (fv v1) && (fv v2)) (edgeList g)
    edgesOut  = edges $ filter (\(v1,v2) -> (fv v1) && (not(fv v2))) (edgeList g)
    cutEdges  = if edgesOut == empty then [] else [edgesOut]
    (foo,bar) = createPartitions g ps

{-
    problems with the above
        * once we're at the stage that there can be no sub-graph with no edges in it, the "overlay vs" stuff is unnecessary
            (we may never be there)
    -- Use source/sink type operators for these sub-graphs
    -- in the hope we can reliably identify them later (real streamgraphs should
    -- only have source nodes at the start and sink nodes at the end)
 -}

unPartition :: Ord a => ([Graph (StreamVertex a)], [Graph (StreamVertex a)]) -> Graph (StreamVertex a)
unPartition (a,b) = foldl overlay Empty (a ++ b)


test_reform_s0 = assertEqual s0 (unPartition $ createPartitions s0 [[0],[1]])
test_reform_s1 = assertEqual s1 (unPartition $ createPartitions s1 [[0,1],[2]])
test_reform_s1_2 = assertEqual s1 (unPartition $ createPartitions s1 [[0],[1,2]])

main = htfMain htf_thisModulesTests
