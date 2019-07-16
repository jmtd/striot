{-# OPTIONS_GHC -F -pgmF htfpp #-}
import Striot.CompileIoT
import Algebra.Graph
import Test.Framework hiding ((===))

import TaxiGraph -- symlink to examples/taxi/generate.hs, provides taxiQ1

import VizGraph

-- attempt to encode a graph transformation!

-- utility/boilerplate -------------------------------------------------------

pp = foldg "()" (show.operator) (wrap " + ") (wrap " * ")
    where wrap x y z = y ++ x ++ z

ppp :: StreamGraph -> String
ppp = foldg "()" (show) (wrap " + ") (wrap " * ")
    where wrap x y z = y ++ x ++ z
          bracket x = "("++x++")"

main = htfMain htf_Main_thisModulesTests

-- applying encoded rules and their resulting ReWriteOps ----------------------

type RewriteRule = StreamGraph -> Maybe (StreamGraph -> StreamGraph)

applyRule :: RewriteRule -> StreamGraph -> StreamGraph
applyRule f g =
    let ops = firstMatch f g in
    case ops of
        Nothing -> g
        Just f  -> f g

-- recursively attempt to apply the rule to the graph, but stop
-- as soon as we get a match
firstMatch :: RewriteRule -> StreamGraph -> Maybe (StreamGraph -> StreamGraph)
firstMatch f g = let r = f g in
    case r of
        Just f    -> Just f
        otherwise -> case g of
            Empty       -> Nothing
            Vertex v    -> Nothing
            Overlay a b -> case firstMatch f a of
                                Just f  -> Just f
                                Nothing -> firstMatch f b
            Connect a b -> case firstMatch f a of
                                Just f  -> Just f
                                Nothing -> firstMatch f b

-- example encoded rules -----------------------------------------------------

-- streamFilter f . streamFilter g = streamFilter (\x -> f x && g x)
filterFuse :: RewriteRule
filterFuse (Connect (Vertex a@(StreamVertex i Filter (p:_) ty _))
                    (Vertex b@(StreamVertex _ Filter (q:_) _ _))) =

    let c = StreamVertex i Filter ["\\p q x -> p x && q x", p, q] ty ty
    in  Just (removeEdge c c . mergeVertices (`elem` [a,b]) c)

filterFuse _ = Nothing

-- streamFilter p . streamMap f = streamMap f . streamFilter (p . f)
mapFilter :: RewriteRule
mapFilter (Connect (Vertex m@(StreamVertex i Map (f:fs) intype _))
                   (Vertex f1@(StreamVertex j Filter (p:ps) _ _))) =

    let f2 = StreamVertex j Filter (("("++p++").("++f++")"):ps) intype intype
    in  Just (replaceVertex f1 m . replaceVertex m f2)

mapFilter _ = Nothing

-- tests ---------------------------------------------------------------------

m1 = Vertex $ StreamVertex 0 Map ["show"] "Int" "String"
f1 = Vertex $ StreamVertex 1 Filter ["\\x -> length x <3"] "String" "String"

m2 = Vertex $ StreamVertex 0 Map ["show"] "Int" "String"
f2 = Vertex $ StreamVertex 1 Filter ["(\\x -> length x <3).(show)"] "Int" "Int"

mapFilterPre = m1 `Connect` f1
mapFilterPost = f2 `Connect` m2

test_mapfilter2 = assertEqual mapFilterPost
    $ applyRule mapFilter mapFilterPre

-- test it finds matches in sub-graphs
mapFilterSub = mapFilterPre `Overlay` Empty
test_mapfilter3 = assertEqual mapFilterPost
    $ applyRule mapFilter mapFilterSub

-- deeper sub-graphs and some redundancy
mapFilterSub2 = Empty `Overlay` Empty `Overlay` mapFilterPre `Overlay` mapFilterPre
test_mapfilter4 = assertEqual mapFilterPost
    $ applyRule mapFilter mapFilterSub2

f3 = Vertex $ StreamVertex 0 Filter ["(>3)"] "String" "String"
f4 = Vertex $ StreamVertex 1 Filter ["(<5)"] "String" "String"
filterFusePre = Connect f3 f4
filterFusePost = Vertex $ StreamVertex 0 Filter
    ["\\p q x -> p x && q x","(>3)","(<5)"] "String" "String"

test_filterFuse = assertEqual (applyRule filterFuse filterFusePre)
    filterFusePost

display1 = displayGraph $ taxiQ1
display2 = displayGraph $ applyRule filterFuse taxiQ1
display3 = displayGraph $ applyRule mapFilter $ applyRule filterFuse taxiQ1

