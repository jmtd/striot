{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-
    attempt to recast CompileIoT in terms of Alga
    Alga has a parameterized type Graph a, so we need to figure out what a
    should be such that vertices are uniquely identified

    confirm the hypothesis that Graph x == Graph x for any concrete x, or
    a graph comprising two Vertices x where x == x are considered the same
    vertex
 -}

module Striot.CompileIoTAgda () where

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
    } deriving (Show,Eq)

instance Ord a => Ord (StreamVertex a) where
    compare (StreamVertex x _) (StreamVertex y _) = compare x y

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


createPartitions :: Ord a => Graph (StreamVertex a) -> PartitionMap -> [(Graph (StreamVertex a))]
createPartitions _ [] = []
createPartitions g (p:ps) = (overlay vs es) : (createPartitions g ps) where
    fv = \v -> (vertexId v) `elem` p
    vs = vertices $ filter fv (vertexList g)
    es = edges $ filter (\(v1,v2) -> (fv v1) && (fv v2)) (edgeList g)

unPartition :: Ord a => [Graph (StreamVertex a)] -> Graph (StreamVertex a)
unPartition = foldl overlay Empty

toDot :: Ord a => Graph (StreamVertex a) -> String
toDot g = "digraph {\n" ++ (vertexDefs) ++ (toDot' (edgeList g)) ++ "}\n" where
    toDot' [] = ""
    toDot' (e:es) = (blah e) ++ (toDot' es)
    blah (v1,v2) = "\t" ++ (show (vertexId v1)) ++ " -> " ++ (show (vertexId v2)) ++ ";\n"
    vertexDefs = concat $ map ((\x -> "\t" ++ x ++ ";\n").show.vertexId) (vertexList g)

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
