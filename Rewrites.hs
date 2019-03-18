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

applyRewriteOps :: StreamGraph -> [ReWriteOp] -> StreamGraph
applyRewriteOps g [] = g
applyRewriteOps g ((ReplaceNode old new):rs) = applyRewriteOps (replaceVertex old new g) rs
applyRewriteOps g ((MergeNode l r):rs) = applyRewriteOps (mergeVertices (\v->v`elem`[l,r]) r g) rs
applyRewriteOps g ((DeleteNode v):rs) = applyRewriteOps (removeVertex v g) rs

type RewriteRule = StreamGraph -> [ReWriteOp]

applyRule :: RewriteRule -> StreamGraph -> StreamGraph
applyRule f g =
    let ops = renameMe f g in
    case ops of
        Nothing   -> g
        Just ops' -> applyRewriteOps g ops'

-- need to recursively attempt to apply the rule to the graph, but stop
-- as soon as we get a match
renameMe :: RewriteRule -> StreamGraph -> Maybe [ReWriteOp]
renameMe f g = let r = f g in
    case r of
        [] -> case g of
            Empty       -> Nothing
            Vertex v    -> Nothing
            Overlay a b -> if renameMe f a /= Nothing then renameMe f a else renameMe f b
            Connect a b -> if renameMe f a /= Nothing then renameMe f a else renameMe f b
        otherwise -> Just r

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

m1 = Vertex $ StreamVertex 0 Map ["show"] "Int" "String"
f1 = Vertex $ StreamVertex 1 Filter ["\\x -> length x <3"] "String" "String"

m2 = Vertex $ StreamVertex 0 Map ["show"] "Int" "String"
f2 = Vertex $ StreamVertex 1 Filter ["(\\x -> length x <3).(show)"] "Int" "Int"

mapFilterPre = m1 `Connect` f1
mapFilterPost = f2 `Connect` m2

test_mapfilter1 = assertEqual mapFilterPost
    $ applyRewriteOps mapFilterPre (mapFilter mapFilterPre)

test_mapfilter2 = assertEqual mapFilterPost
    $ applyRule mapFilter mapFilterPre
