{-# OPTIONS_GHC -F -pgmF htfpp #-}
import Striot.CompileIoT
import Algebra.Graph
import Test.Framework hiding ((===))

-- attempt to encode a graph transformation!

-- utility/boilerplate -------------------------------------------------------

pp = foldg "()" (show.operator) (wrap " + ") (wrap " * ")
    where wrap x y z = y ++ x ++ z

ppp :: StreamGraph -> String
ppp = foldg "()" (show) (wrap " + ") (wrap " * ")
    where wrap x y z = y ++ x ++ z
          bracket x = "("++x++")"

-- attempt to make defining these more compact
sv a b c d e = Vertex (StreamVertex a b c d e)

main = htfMain htf_Main_thisModulesTests

-- applying encoded rules and their resulting ReWriteOps ----------------------

data ReWriteOp = ReplaceNode StreamVertex StreamVertex
               | MergeNode   StreamVertex StreamVertex
               | DeleteNode  StreamVertex
               deriving (Show,Eq)

-- XXX needs reworking for (StreamGraph -> [ReWriteOp])
-- XXX rename
apply :: (StreamGraph -> StreamGraph) -> StreamGraph -> StreamGraph
apply f Empty = Empty
apply f (Vertex v) = Vertex v
apply f g@(Overlay x y) = if   g == f g
                          then Overlay (f x) (f y)
                          else g
apply f g@(Connect x y) = f g

-- XXX rename
doIt :: StreamGraph -> [ReWriteOp] -> StreamGraph
doIt g [] = g
doIt g ((ReplaceNode old new):rs) = doIt (replaceVertex old new g) rs
doIt g ((MergeNode l r):rs) = doIt (mergeVertices (\v->v`elem`[l,r]) r g) rs
doIt g ((DeleteNode v):rs) = doIt (removeVertex v g) rs

-- example encoded rules -----------------------------------------------------

-- streamFilter f . streamFilter g = streamFilter (\x -> f x && g x)
filterFuse :: StreamGraph -> [ReWriteOp]
filterFuse (Connect (Vertex a@(StreamVertex i Filter (f1:_) ty _))
                    (Vertex b@(StreamVertex _ Filter (f2:_) _ _))) =

    let c = StreamVertex i Filter ["\\f g x -> f x && g x", f1, f2, "s"] ty ty
    in  [ ReplaceNode b c
        , MergeNode a c ]
filterFuse g = []

-- streamFilter p . streamMap f = streamMap f . streamFilter (p . f)
mapFilter :: StreamGraph -> [ReWriteOp]
mapFilter (Connect (Vertex m@(StreamVertex i Map (f:fs) intype _))
                   (Vertex f1@(StreamVertex j Filter (p:ps) _ _))) =
    let f2 = StreamVertex j Filter (("("++p++").("++f++")"):ps) intype intype
    in  [ ReplaceNode m f2
        , ReplaceNode f1 m
        ]
mapFilter g = []


-- tests ---------------------------------------------------------------------

-- (need to revamp these entirely)

m1 = Vertex $ StreamVertex 0 Map ["show"] "Int" "String"
f1 = Vertex $ StreamVertex 1 Filter ["\\x -> length x <3"] "String" "String"

m2 = Vertex $ StreamVertex 0 Map ["show"] "Int" "String"
f2 = Vertex $ StreamVertex 1 Filter ["(\\x -> length x <3).(show)"] "Int" "Int"

mapFilterEx6 = m1 `Connect` f1
correct = f2 `Connect` m2

test_foo = assertEqual correct (doIt mapFilterEx6 (mapFilter mapFilterEx6))
