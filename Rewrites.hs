{-# OPTIONS_GHC -F -pgmF htfpp #-}
import Striot.CompileIoT
import Algebra.Graph
import Test.Framework hiding ((===))

import TaxiGraph -- symlink to examples/taxi/generate.hs, provides taxiQ1

import VizGraph

-- attempt to encode a graph transformation!

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

-- streamFilter f . streamFilter g = streamFilter (\x -> f x && g x) ---------

filterFuse :: RewriteRule
filterFuse (Connect (Vertex a@(StreamVertex i Filter (p:_) ty _))
                    (Vertex b@(StreamVertex _ Filter (q:_) _ _))) =

    let c = StreamVertex i Filter ["\\p q x -> p x && q x", p, q] ty ty
    in  Just (removeEdge c c . mergeVertices (`elem` [a,b]) c)

filterFuse _ = Nothing

f3 = Vertex $ StreamVertex 0 Filter ["(>3)"] "String" "String"
f4 = Vertex $ StreamVertex 1 Filter ["(<5)"] "String" "String"
filterFusePre = Connect f3 f4
filterFusePost = Vertex $ StreamVertex 0 Filter
    ["\\p q x -> p x && q x","(>3)","(<5)"] "String" "String"

test_filterFuse = assertEqual (applyRule filterFuse filterFusePre)
    filterFusePost

-- streamFilter p . streamMap f = streamMap f . streamFilter (p . f) ---------

mapFilter :: RewriteRule
mapFilter (Connect (Vertex m@(StreamVertex i Map (f:fs) intype _))
                   (Vertex f1@(StreamVertex j Filter (p:ps) _ _))) =

    let f2 = StreamVertex j Filter (("("++p++").("++f++")"):ps) intype intype
    in  Just (replaceVertex f1 m . replaceVertex m f2)

mapFilter _ = Nothing

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

-- streamFilter >>> streamFilterAcc f a q ------------------------------------

filterFilterAcc :: RewriteRule
filterFilterAcc (Connect (Vertex v1@(StreamVertex i Filter (p:_) ty _))
                         (Vertex v2@(StreamVertex _ FilterAcc (f:a:q:_) _ _))) =
    let v3 = StreamVertex i FilterAcc [ "(let p = ("++p++"); f = ("++f++") in \\a v -> if p v then f a v else a)"
                                      , a
                                      , "(let p = ("++p++"); q = ("++q++") in \\v a -> p v && q v a)"
                                      ] ty ty
    in  Just (removeEdge v3 v3 . mergeVertices (`elem` [v1,v2]) v3)
filterFilterAcc _ = Nothing

filterFilterAccPre = Vertex (StreamVertex 3 Filter ["p"] "Int" "Int")
                     `Connect`
                     Vertex (StreamVertex 2 FilterAcc ["f", "a", "q"] "Int" "Int")
filterFilterAccPost = Vertex $
  StreamVertex 3 FilterAcc [ "(let p = (p); f = (f) in \\a v -> if p v then f a v else a)"
                           , "a"
                           , "(let p = (p); q = (q) in \\v a -> p v && q v a)"
                           ] "Int" "Int"

test_filterFilterAcc = assertEqual (applyRule filterFilterAcc filterFilterAccPre)
    filterFilterAccPost

-- streamFilterAcc >>> streamFilter ------------------------------------------

filterAccFilter :: RewriteRule
filterAccFilter (Connect (Vertex v1@(StreamVertex i FilterAcc (f:a:p:_) ty _))
                         (Vertex v2@(StreamVertex _ Filter (q:_) _ _))) =
    let p' = "(let p = ("++p++"); q = ("++q++") in \\v a -> p v a && q v)"
        v  = StreamVertex i FilterAcc [f,a,p'] ty ty
    in  Just (removeEdge v v . mergeVertices (`elem` [v1,v2]) v)

filterAccFilterPre  = Vertex (StreamVertex 1 FilterAcc ["f","a","p"] "Int" "Int")
                      `Connect`
                      Vertex (StreamVertex 2 Filter ["q"] "Int" "Int")
filterAccFilterPost = Vertex $
    StreamVertex 1 FilterAcc [ "f", "a"
                             , "(let p = (p); q = (q) in \\v a -> p v a && q v)"
                             ] "Int" "Int"

test_filterAccFilter = assertEqual (applyRule filterAccFilter filterAccFilterPre)
    filterAccFilterPost

-- streamFilterAcc >>> streamFilterAcc ---------------------------------------

filterAccFilterAcc :: RewriteRule
filterAccFilterAcc (Connect (Vertex v1@(StreamVertex i FilterAcc (f:a:p:_) ty _))
                            (Vertex v2@(StreamVertex _ FilterAcc (g:b:q:_) _ _))) =
    let f' = "(let f = ("++f++"); p = ("++p++"); g = ("++g++") in\
             \ \\ (a,b) v -> (f a v, if p v a then g b v else b))"
        a' = "("++a++","++b++")"
        q' = "(let p = ("++p++"); q = ("++q++") in \\v (y,z) -> p v y && q v z)"
        v  = StreamVertex i FilterAcc [f',a',q'] ty ty
    in  Just (removeEdge v v . mergeVertices (`elem` [v1,v2]) v)

filterAccFilterAccPre  = Vertex (StreamVertex 1 FilterAcc ["f","a","p"] "Int" "Int")
                         `Connect`
                         Vertex (StreamVertex 2 FilterAcc ["g","b","q"] "Int" "Int")
filterAccFilterAccPost = Vertex $
    StreamVertex 1 FilterAcc [ "(let f = (f); p = (p); g = (g) in \\ (a,b) v -> (f a v, if p v a then g b v else b))"
                             , "(a,b)"
                             , "(let p = (p); q = (q) in \\v (y,z) -> p v y && q v z)"
                             ] "Int" "Int"
test_filterAccFilterAcc = assertEqual (applyRule filterAccFilterAcc filterAccFilterAccPre)
    filterAccFilterAccPost

-- streamMap >>> streamMap ---------------------------------------------------

mapFuse :: RewriteRule
mapFuse (Connect (Vertex v1@(StreamVertex i Map (f:_) t1 _))
                 (Vertex v2@(StreamVertex _ Map (g:_) _ t2))) =
    let v = StreamVertex i Map ["(let f = ("++f++"); g = ("++g++") in (f >>> g))"] t1 t2
    in  Just (removeEdge v v . mergeVertices (`elem` [v1,v2]) v)

mapFusePre = Vertex (StreamVertex 0 Map ["show"] "Int" "String") `Connect`
  Vertex (StreamVertex 1 Map ["length"] "String" "Int")
mapFusePost = Vertex $ StreamVertex 0 Map ["(let f = (show); g = (length) in (f >>> g))"] "Int" "Int"
test_mapFuse = assertEqual (applyRule mapFuse mapFusePre) mapFusePost

-- utility/boilerplate -------------------------------------------------------

pp = foldg "()" (show.operator) (wrap " + ") (wrap " * ")
    where wrap x y z = y ++ x ++ z

ppp :: StreamGraph -> String
ppp = foldg "()" (show) (wrap " + ") (wrap " * ")
    where wrap x y z = y ++ x ++ z
          bracket x = "("++x++")"

main = htfMain htf_Main_thisModulesTests

display1 = displayGraph $ taxiQ1
display2 = displayGraph $ applyRule filterFuse taxiQ1
display3 = displayGraph $ applyRule mapFilter $ applyRule filterFuse taxiQ1
