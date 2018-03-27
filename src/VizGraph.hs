module VizGraph(drawPartitionedStreamGraph,drawStreamGraph) where
import Data.List
import Striot.CompileIoT (printParams, graphEdgesWithTypes, Id, Partition, StreamGraph, opid, operations, parameters, operator, s0, s1)

import qualified Striot.CompileIoTAlga as CA
import Algebra.Graph

import Data.GraphViz
import Data.Text.Lazy (Text, pack, unpack)
import qualified Data.Map as Map
import Data.GraphViz.Printing (toDot, renderDot)
import Data.GraphViz.Attributes.Complete



-- https://hackage.haskell.org/package/graphviz-2999.18.1.2/docs/Data-GraphViz-Attributes-Complete.html#t:Attributes
-- https://hackage.haskell.org/package/graphviz-2999.18.1.2/docs/Data-GraphViz.html
-- http://haroldcarr.com/posts/2014-02-28-using-graphviz-via-haskell.html

-------------

streamGraphToDotGraph:: StreamGraph -> (GraphvizParams Int String String () String) -> Data.GraphViz.DotGraph Int
streamGraphToDotGraph sg params = graphElemsToDot params
                                                  (map (\op -> (opid op,(show $ operator op) ++ " " ++ printParams (parameters op))) (operations sg)) -- nodes -- could remove printParams if operation only is needed
                                                  (map (\(sourceNode,(destNode,destPort),oType) -> (sourceNode,destNode,oType)) (graphEdgesWithTypes sg)) -- edges

---------
-- https://hackage.haskell.org/package/graphviz-2999.18.0.0/docs/Data-GraphViz.html#t:GraphvizParams
myParams :: GraphvizParams Int String String () String
myParams = nonClusteredParams {
--4. Let the graphing engine know that we want the edges to be directed arrows
-- as follows:
              isDirected = True
--5. Set our own global attributes for a graph, node, and edge appearance as follows:
            , globalAttributes = [myGraphAttrs, myNodeAttrs, myEdgeAttrs]
--6. Format nodes in our own way as follows:
            , fmtNode = myFN
--7. Format edges in our own way as follows:
            , fmtEdge = myFE
            }
--8. Define the customizations as shown in the following code snippet:
           where myGraphAttrs = GraphAttrs [ -- RankDir FromLeft
                                            BgColor [toWColor White] ]
                 myNodeAttrs =  NodeAttrs [ Shape BoxShape
                                          , FillColor [toWColor White]
                                          , Style [SItem Filled []] ]
                 myEdgeAttrs =  EdgeAttrs [ Weight (Int 10)
                                          , Color [toWColor Black]
                                          , FontColor (toColor Black) ]
                 myFN (n,l)   = [(Label . StrLabel) (Data.Text.Lazy.pack (drop 6 l))] -- pack converts the strings used in streamGraph to Text used by GraphvizParams 
                                                                                      -- drop 6 removes the "Stream " prefix
                 myFE (f,t,l) = [(Label . StrLabel) (Data.Text.Lazy.pack (' ':(drop 7 l)))] -- drop 7 removes the "Stream" prefix from the operators
                 
-- http://goto.ucsd.edu/~rjhala/llvm-haskell/doc/html/llvm-analysis/src/LLVM-Analysis-CFG.html gives a helpful example
clusteredParams ::  GraphvizParams Int String String Int String
clusteredParams = defaultParams {
--4. Let the graphing engine know that we want the edges to be directed arrows
-- as follows:
              isDirected = True
--5. Set our own global attributes for a graph, node, and edge appearance as follows:
            , globalAttributes = [myGraphAttrs, myNodeAttrs, myEdgeAttrs]
--6. Format nodes in our own way as follows:
            , fmtNode    = myFN
--7. Format edges in our own way as follows:
            , fmtEdge    = myFE
-- Format clusters:
            , clusterBy  = clustBy
            , clusterID  = Num . Int
            , fmtCluster = myFC
            }
--8. Define the customizations as shown in the following code snippet:
           where myGraphAttrs  = GraphAttrs [ -- RankDir FromLeft
                                            BgColor [toWColor White] ]
                 myNodeAttrs   =  NodeAttrs [ Shape BoxShape
                                          , FillColor [toWColor White]
                                          , Style [SItem Filled []] ]
                 myEdgeAttrs   =  EdgeAttrs [ Weight (Int 10)
                                          , Color [toWColor Black]
                                          , FontColor (toColor Black) ]
                 myFN (n,l)    = [(Label . StrLabel) (Data.Text.Lazy.pack (drop 6 l))] -- pack converts the strings used in streamGraph to Text used by GraphvizParams 
                                                                                      -- drop 6 removes the "Stream " prefix
                 myFE (f,t,l)  = [(Label . StrLabel) (Data.Text.Lazy.pack (' ':(drop 7 l)))] -- drop 7 removes the "Stream" prefix from the operators    
                 clustBy (n,l) = C (n `mod` 2) $ N (n,l)
                 myFC m        = [GraphAttrs [toLabel $ "n == " ++ show m ++ " (mod 2)"]]


clStreamGraphToDotGraph:: StreamGraph -> (GraphvizParams Int String String Int String) -> Data.GraphViz.DotGraph Int
clStreamGraphToDotGraph sg params = graphElemsToDot params
                                                  (map (\op -> (opid op,(show $ operator op) ++ " " ++ printParams (parameters op))) (operations sg)) -- nodes -- could remove printParams if operation only is needed
                                                  (map (\(sourceNode,(destNode,destPort),oType) -> (sourceNode,destNode,oType)) (graphEdgesWithTypes sg)) -- edges

clusteredParams2 :: Map.Map Id Partition -> GraphvizParams Int String String Int String
clusteredParams2 idToPart = defaultParams {
              isDirected = True
            , globalAttributes = [myGraphAttrs, myNodeAttrs, myEdgeAttrs]
            , fmtNode    = myFN
            , fmtEdge    = myFE
            , clusterBy  = clustBy
            , clusterID  = Num . Int
            , fmtCluster = myFC
            }
           where myGraphAttrs  = GraphAttrs [ -- RankDir FromLeft
                                            BgColor [toWColor White] ]
                 myNodeAttrs   =  NodeAttrs [ Shape BoxShape
                                          , FillColor [toWColor White]
                                          , Style [SItem Filled []] ]
                 myEdgeAttrs   =  EdgeAttrs [ Weight (Int 10)
                                          , Color [toWColor Black]
                                          , FontColor (toColor Black) ]
                 myFN (n,l)    = [(Label . StrLabel) (Data.Text.Lazy.pack (drop 6 l))] -- pack converts the strings used in streamGraph to Text used by GraphvizParams 
                                                                                       -- drop 6 removes the "Stream " prefix
                 myFE (f,t,l)  = [(Label . StrLabel) (Data.Text.Lazy.pack (' ':(drop 7 l)))] -- drop 7 removes the "Stream" prefix from the operators    
                 clustBy (n,l) = C (idToPart Map.! n) $ N (n,l)
                 myFC p        = [GraphAttrs [toLabel $ "Partition " ++ show p]]

drawPartitionedStreamGraph::  StreamGraph -> [(Partition,[Id])] -> String -> IO FilePath
drawPartitionedStreamGraph sg parts filename = addExtension (runGraphviz (clStreamGraphToDotGraph sg (clusteredParams2 (mkPartMap parts)))) Png filename

mkPartMap:: [(Partition,[Id])] -> Map.Map Id Partition
mkPartMap ps = foldl (\m (i,p) -> Map.insert i p m) Map.empty (concatMap (\(p,ids)->[(i,p)|i<-ids]) ps)

drawStreamGraph:: StreamGraph -> String -> IO FilePath
drawStreamGraph sg filename = addExtension (runGraphviz (streamGraphToDotGraph sg myParams)) Png filename 

------------------------------------------------------------------------------

-- XXX: Algebra.Graph.Export.Dot might be better; but means calling dot yourself rather
-- than using the Dot library, unless ther's a helper to convert a String representation
-- of a dot file output to a Dot datastructure

-- override some parameters to avoid chopping up strings
algaParams = myParams { fmtNode = myFN, fmtEdge = myFE } where
    myFN (n,l) = [(Label . StrLabel) (Data.Text.Lazy.pack l)]
    myFE (f,t,l) = [(Label . StrLabel) (Data.Text.Lazy.pack (' ':l))]

-- tightened type up to String instead a; need => Ord a
drawStreamGraphAlga :: Graph (CA.StreamVertex String) -> String -> IO FilePath
drawStreamGraphAlga sg filename = addExtension (runGraphviz (algaGraphToDotGraph sg algaParams)) Png filename

-- tightened type up to String instead a; need => Ord a
algaGraphToDotGraph :: Graph (CA.StreamVertex String) -> (GraphvizParams Int String String () String) -> Data.GraphViz.DotGraph Int
algaGraphToDotGraph g params = graphElemsToDot params nodes edges where
    nodes = map (\v -> (CA.vertexId v, show v)) $ vertexList g
    edges = map (\e -> ((CA.vertexId.fst) e, (CA.vertexId.snd) e, (show.(CA.operator).fst) e)) $ edgeList g
    -- XXX third arg is type, we're just showing operator for now

main = do
    drawStreamGraphAlga CA.s2 "s2"

-- XXX also need to modify the params, which are stripping/dropping chars from the labels
