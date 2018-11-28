{-# OPTIONS_GHC -F -pgmF htfpp #-}
import Striot.CompileIoT
import Algebra.Graph
import Test.Framework hiding ((===))

-- attempt to encode a graph transformation!

-- streamFilter f . streamFilter g
-- =
-- streamFilter (\x -> f x && g x)

-- :: Graph (StreamVertex ["String"])
lhs = path
    [ StreamVertex 0 Filter ["f","s"] "a"
    , StreamVertex 1 Filter ["g","s"] "a"
    ]

-- RHS as a function
-- in fact this encodes the whole thing: how to match and what rewrite to apply
rhs :: StreamGraph -> StreamGraph
rhs (Connect (Vertex (StreamVertex i Filter (f1:_) intype))
             (Vertex (StreamVertex _ Filter (f2:_) _))) =
    Vertex $ StreamVertex i Filter ["\\f g x -> f x && g x", f1, f2, "s"] intype
-- perhaps, we could use pattern matching to both check *and* apply rewrites,
-- with a catch-all pattern for "no match, no op"
rhs2 g = g

exampleStream = path
    [ StreamVertex 1 Source ["return 6"]                         "String"
    , StreamVertex 2 Filter ["(\\i -> (read i :: Int) > 5)", "s"] "String"
    , StreamVertex 3 Filter ["(\\i -> (read i :: Int) <= 10)", "s"] "String"
    , StreamVertex 4 Sink   ["mapM_ $ putStrLn . (\"receiving \"++) . show . value"] "[String]"
    ]

entryNodes g = let
    vl = vertexList g
    el = edgeList g
    sinks = map snd el
    in filter (\v -> not (v `elem` sinks)) vl

--doesRuleMatch :: Graph StreamVertex -> Graph StreamVertex -> Bool
--doesRuleMatch g lhs = let
--    -- find the entrance node (nodes?) into the lhs

pp = foldg "()" (show.operator) (wrap " + ") (wrap " * ")
    where wrap x y z = y ++ x ++ z

ppp :: StreamGraph -> String
ppp = foldg "()" (show) (wrap " + ") (wrap " * ")
    where wrap x y z = y ++ x ++ z
          bracket x = "("++x++")"

-- perhaps we should just try to exhaustively apply the function to every subgraph
-- in the graph? subgraph of size 2 that is

-- attempt to make defining these more compact
sv a b c d = Vertex (StreamVertex a b c d)

-- some more encoded rules
-- streamFilter p . streamMap f = streamMap f . streamFilter (p . f)
mapFilter :: StreamGraph -> StreamGraph
mapFilter (Connect mapv@(Vertex (StreamVertex i Map (f:fs) intype))
                   (Vertex (StreamVertex j Filter (p:ps) _)))
    = sv j Filter (("("++p++").("++f++")"):ps) intype `Connect` mapv
mapFilter g = g

mapFilterEx = (sv 0 Map ["f"] "Int") `Connect` (sv 1 Filter ["p"] "String")
mapFilterEx2 = (sv 0 Map ["show"] "Int") `Connect`
                       (sv 1 Filter ["\\x -> length x <3"] "String")

-- adjusted structure to see if we can thwart pattern matching
mapFilterEx3 = empty `overlay` mapFilterEx2
test_ex3_thwart          = assertEqual mapFilterEx3 (mapFilter mapFilterEx3)
-- "simplify" fixes this
test_simplify_ex3        = assertNotEqual mapFilterEx3 (mapFilter (simplify mapFilterEx3))

-- the above would not be thwarted if the function was applied to all subgraphs,
-- but would be by this version…
mapFilterEx4 = (sv 0 Map ["show"] "Int") `Connect`
                       (empty `Overlay` sv 1 Filter ["\\x -> length x <3"] "String")

test_ex2_4_equiv         = assertEqual mapFilterEx2 mapFilterEx4
test_mapfilter_fails_ex4 = assertEqual mapFilterEx4 (mapFilter mapFilterEx4)
-- simplify fixes this, too
test_simplify_ex2_ex4    = assertBool $ mapFilterEx2 === simplify mapFilterEx4
test_mapfilter_ex4       = assertNotEqual mapFilterEx4 (mapFilter (simplify mapFilterEx4))


main = htfMain htf_Main_thisModulesTests
