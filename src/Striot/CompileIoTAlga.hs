{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-
    attempt to recast CompileIoT in terms of Alga
    Alga has a parameterized type Graph a, so we need to figure out what a
    should be such that vertices are uniquely identified
 -}

module Striot.CompileIoTAlga ( createPartitions    -- used by Optimizer
                             , StreamGraph(..)     -- used by Optimizer
                          -- , StreamOperation(..) -- used by Optimizer
                             , StreamVertex(..)
                             , StreamOperator(..)  -- used by Optimizer
                          -- , Partition           -- used by Optimizer
                          -- , Id
                          -- , createPartitionsAndEdges
                          -- , graphEdgesWithTypes -- used by VizGraph
                          -- , printParams         -- used by VizGraph
                             , generateCode
                             , PartitionMap -- implementation feature of this module
                             , htf_thisModulesTests
                             , s0, s1
                             ) where

import Algebra.Graph
import Test.Framework
import Data.List -- intersperse

-- decorating with comments corresponding to valence/def in the draft paper

data StreamOperator   = Map       -- (a -> b)      -> Stream a           -> Stream b
                      | Filter    -- (a -> Bool)   -> Stream a           -> Stream a
                      | Expand    -- Stream [a]    -> Stream a
                      | Window    -- (Stream a     -> Stream [a])        -> Stream a      -> Stream [a]
                      | Merge     -- [Stream a]    -> Stream a
                      | Join      -- Stream a      -> Stream b           -> Stream (a,b)
                      | Scan      -- (b -> a -> b) -> b                  -> Stream a      -> Stream b                  -- "stream map with accumulating parameter"
                      | FilterAcc -- (b -> a -> b) -> b (a -> b -> Bool)                                                                 -> Stream a -> Stream a
                      | Source
                      | Sink   deriving (Ord,Eq)

instance Show StreamOperator where
    show Map             = "streamMap"
    show Filter          = "streamFilter"
    show Window          = "streamWindow"
    show Merge           = "streamMerge"
    show Join            = "streamJoin"
    show Scan            = "streamScan"
    show FilterAcc       = "streamFilterAcc"
    show Expand          = "streamExpand"
    show Source          = "streamSource"
    show Sink            = "streamSink"

-- Id needed for uniquely identifying a vertex. (Is there a nicer way?)
data StreamVertex = StreamVertex
    { vertexId   :: Int
    , operator   :: StreamOperator
    , parameters :: [String] -- XXX strings of code. From CompileIoT. Variable length e.g.FilterAcc takes 3 (?)
    , intype     :: String
    } deriving (Eq)

instance Ord StreamVertex where
    compare (StreamVertex x _ _ _) (StreamVertex y _ _ _) = compare x y

instance Show StreamVertex where
    show v = (show$vertexId v)++(map (\x->if x=='"' then '\'' else x) (show (operator v))) ++
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
-- XXX: we could probably fold the cut edges and return ([Graph], Graph)
createPartitions :: Graph StreamVertex -> PartitionMap -> ([Graph StreamVertex], [Graph StreamVertex])
createPartitions _ [] = ([],[])
createPartitions g (p:ps) = ((overlay vs es):tailParts, cutEdges ++ tailCuts) where
    vs        = vertices $ filter fv (vertexList g)
    es        = edges $ filter (\(v1,v2) -> (fv v1) && (fv v2)) (edgeList g)
    cutEdges  = if edgesOut == empty then [] else [edgesOut]
    fv v      = (vertexId v) `elem` p
    edgesOut  = edges $ filter (\(v1,v2) -> (fv v1) && (not(fv v2))) (edgeList g)
    (tailParts, tailCuts) = createPartitions g ps

unPartition :: ([Graph StreamVertex], [Graph StreamVertex]) -> Graph StreamVertex
unPartition (a,b) = foldl overlay Empty (a ++ b)

type StreamGraph = Graph StreamVertex

------------------------------------------------------------------------------
-- quickcheck experiment

instance Arbitrary StreamOperator where
    arbitrary = elements [ Map , Filter , Expand , Window , Merge , Join , Scan
                         , FilterAcc , Source , Sink ]

instance Arbitrary StreamVertex where
    arbitrary = do
        vertexId <- arbitrary
        operator <- arbitrary
        let parameters = []
            intype = "String"
            in
                return $ StreamVertex vertexId operator parameters intype

streamgraph :: Gen StreamGraph
streamgraph = sized streamgraph'
streamgraph' 0 = return g where g = empty :: StreamGraph
streamgraph' n | n>0 = do
    v <- arbitrary
    t <- streamgraph' (n-1)
    return $ connect (vertex v) t

--instance Arbitrary StreamGraph where
--    arbitrary = do
--        -- XXX build a random StreamGraph
--        x <- arbitrary
--        return $ ...

------------------------------------------------------------------------------

{-
    a well-formed streamgraph:
        always starts with a Source?
        always has just one Source?
        always ends with a Sink?
        always has just one Sink?
        is entirely connected?
        ...
    a well-formed partition spec:
        has ≥ 1 partition
        references node IDs that exist
        covers all node IDs?
        passes some kind of connectedness test?
-}

stdImports = ["Striot.FunctionalIoTtypes", "Striot.FunctionalProcessing", "Striot.Nodes"]

generateCode :: StreamGraph -> PartitionMap -> [String] -> [String]
generateCode sg pm imports = generateCode' (createPartitions sg pm) imports

generateCode' :: ([StreamGraph], [StreamGraph]) -> [String] -> [String]
generateCode' (sgs,cuts) imports = map (generateCodeFromStreamGraph imports cuts) (zip [1..] sgs)

data NodeType = NodeSource | NodeSink | NodeLink deriving (Show)

nodeType :: StreamGraph -> NodeType
nodeType sg = if operator (head (vertexList sg)) == Source
              then NodeSource
              else if (operator.head.reverse.vertexList) sg == Sink
                   then NodeSink
                   else NodeLink

-- vertexList outputs *sorted*. That corresponds to the Id value for
-- our StreamVertex type
generateCodeFromStreamGraph :: [String] -> ([StreamGraph]) -> (Integer,StreamGraph) -> String
generateCodeFromStreamGraph imports cuts (partId,sg) = intercalate "\n" $
    nodeId : -- convenience comment labelling the node/partition ID
    imports' ++
    (possibleSrcSinkFn sg) :
    sgTypeSignature :
    sgIntro :
    (map ((padding++).generateCodeFromVertex) (zip [(valence+1)..] intVerts)) ++
    [padding ++ "in " ++ lastIdentifier,"\n",
    "main :: IO ()",
    nodeFn sg] where
        nodeId = "-- node"++(show partId)
        padding = "    "
        sgTypeSignature = "streamGraphFn ::"++(concat $ take valence $ repeat $ " Stream "++inType++" ->")++" Stream "++outType
        sgIntro = "streamGraphFn "++sgArgs++" = let"
        sgArgs = unwords $ map (('n':).show) [1..valence]
        imports' = (map ("import "++) ("Network":imports)) ++ ["\n"]
        lastIdentifier = 'n':(show $ (length intVerts) + valence)
        intVerts= filter (\x-> not $ operator x `elem` [Source,Sink]) $ vertexList sg
        valence = partValence sg cuts
        nodeFn sg = case (nodeType sg) of
            NodeSource -> generateNodeSrc  (partId + 1)
            NodeLink   -> generateNodeLink (partId + 1)
            NodeSink   -> generateNodeSink valence
        possibleSrcSinkFn sg = case (nodeType sg) of
            NodeSource -> generateSrcFn sg
            NodeLink   -> ""
            NodeSink   -> generateSinkFn sg
        inType = intype $ head $ vertexList sg
        outType= intype $ head $ reverse $ vertexList sg -- XXX not strictly true

generateSrcFn :: StreamGraph -> String
generateSrcFn sg = "src1 :: IO String\nsrc1 = " ++
    (intercalate "\n" $ parameters $ head $ vertexList sg) ++ "\n"

generateSinkFn:: StreamGraph -> String
generateSinkFn sg = "sink1 :: Show a => [a] -> IO ()\nsink1 = " ++
    (intercalate "\n" $ parameters $ head $ reverse $ vertexList sg) ++ "\n"

generateNodeLink n = "main = nodeLink streamGraphFn 9001 \"node"++(show n)++"\" 9001"
generateNodeSrc  n = "main = nodeSource src1 streamGraphFn \"node"++(show n)++"\" 9001"
generateNodeSink v = case v of
    1 -> "main = nodeSink streamGraphFn sink1 9001"
    2 -> "main = nodeSink2 streamGraphFn sink1 9001 9002"
    v -> error "generateNodeSink: unhandled valence " ++ (show v)

-- special case handling needed for several node types, e.g.,
--      merge: parameters will include the opid so no need to suffix
generateCodeFromVertex :: (Int, StreamVertex) -> String
generateCodeFromVertex (opid, v)  = concat [ "n", (show opid), " = "
                                           , show (operator v)
                                           , " ("
                                           , intercalate "\n" (parameters v)
                                           , ") ", ('n':(show (opid-1)))
                                           ]

------------------------------------------------------------------------------
-- tests / test data

-- Source -> Sink
s0 = connect (Vertex (StreamVertex 0 (Source) [] "String"))
    (Vertex (StreamVertex 1 (Sink) [] "String"))

-- Source -> Filter -> Sink
s1 = path [ StreamVertex 0 (Source) [] "String"
          , StreamVertex 1 Filter [] "String"
          , StreamVertex 2 (Sink) [] "String"
          ]

-- temporarily disabled, complaining about:
-- ambuguous type variable... Ord constraint...
test_reform_s0 = assertEqual s0 (unPartition $ createPartitions s0 [[0],[1]])
test_reform_s1 = assertEqual s1 (unPartition $ createPartitions s1 [[0,1],[2]])
test_reform_s1_2 = assertEqual s1 (unPartition $ createPartitions s1 [[0],[1,2]])

-- how many incoming edges to this partition?
-- + how many source nodes
partValence :: StreamGraph -> [StreamGraph] -> Int
partValence g cuts = let
    cut = foldl overlay empty cuts
    verts = vertexList g
    inEdges = filter (\e -> (snd e) `elem` verts) (edgeList cut)
    sourceNodes = filter (\v -> Source == operator v) (vertexList g)
    in
        (length sourceNodes) + (length inEdges)

