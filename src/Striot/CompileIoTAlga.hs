{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-
    attempt to recast CompileIoT in terms of Alga
    Alga has a parameterized type Graph a, so we need to figure out what a
    should be such that vertices are uniquely identified
 -}

--  XXX do we need to export PartitionMap?
module Striot.CompileIoTAlga ( createPartitions
                             , StreamVertex
                             , StreamOperator
                             , htf_thisModulesTests
                             ) where

import Algebra.Graph -- alga
import Test.Framework
-- import qualified Data.Map as Map

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

-- the crux of the issue: what data does a vertex actually need?
-- let's give them an Id to resolve this (for now?)
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
s1 = overlay
         (connect (Vertex (StreamVertex 0 (Source "?"))) (Vertex (StreamVertex 1 Filter)))
         (connect (Vertex (StreamVertex 1 Filter)) (Vertex (StreamVertex 2 (Sink "!"))))

-- attempt to more concisely define s1
s1a = overlay (connect v0 v1) (connect v1 v2) where
    v0 = Vertex $ StreamVertex 0 $ Source "?"
    v1 = Vertex $ StreamVertex 1 $ Filter
    v2 = Vertex $ StreamVertex 2 $ Sink "!"

-- nicer?
s1b = edges [(v0,v1), (v1,v2)] where
    v0 = StreamVertex 0 (Source "?")
    v1 = StreamVertex 1 Filter
    v2 = StreamVertex 2 (Sink "!")

test_same_a = assertEqual s1 s1a
test_same_b = assertEqual s1 s1b

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

toDot :: Ord a => Show a => Graph a -> String
toDot g = "digraph {\n" ++ vertexDefs ++ (toDot' (edgeList g)) ++ "}\n" where
    toDot' [] = ""
    toDot' (e:es) = (blah e) ++ (toDot' es)
    blah (v1,v2) = "\t\"" ++ (show' v1) ++ "\" -> \"" ++ (show' v2) ++ "\";\n"
    vertexDefs = concatMap (\x -> "\t\"" ++ (show' x) ++ "\" [label=\"" ++ (show' x) ++ "\"]" ++ ";\n") (vertexList g)
    show' = escape . show

-- temp until I've written a show that is dot-label-safe
escape :: String -> String
escape [] = []
escape (s:ss) = if   s `elem` escapeme
                then '\\':s:(escape ss)
                else s:(escape ss)
    where
        escapeme = "\\\":"
    --safechars = concat [['a'..'z'],['A'..'Z'],['\200'..'\377'],"_"]


{-
s0 = StreamGraph "jmtdtest" 2 [] [
       StreamOperation 1 [ ] Source ["sourceGen"] "Stream Trip" ["Taxi.hs","SourceGenerator.hs"],
       StreamOperation 2 [1] Sink   ["print"]     ""            []
     ]
-}



{-
data StreamGraph = StreamGraph
   { gid        :: String
   , resultId   :: Id
   , ginputs    :: [(Id,String)] -- the String is the Type of the input
   , operations :: [StreamOperation]}
      deriving (Show, Eq)

data StreamOperation  = StreamOperation
   { opid       :: Int                  => uniqually identify this SO (Vertex)
   , opinputs   :: [Int]                => inbound connection from other SO (edges)
   , operator   :: StreamOperator       => type (Vertex)
   , parameters :: [String]             => depends on type.. generator for Source XXX
   , outputType :: String               => depends...
   , imports    :: [String]}            => ?? (turned into haskell import lines)
     deriving (Show, Eq)

 -}

main = htfMain htf_thisModulesTests
