import Striot.CompileIoT
import Algebra.Graph

-- attempt to encode a graph transformation!

-- streamFilter f . streamFilter g
-- =
-- streamFilter (\x -> f x && g x)

-- :: Graph (StreamVertex ["String"])
lhs = path
    [ StreamVertex 0 Filter ["f","s"] "a"
    , StreamVertex 1 Filter ["g","s"] "a"
    ]
-- XXX type different to lhs
rhs = StreamVertex 0 Filter ["\\x -> f x && g x", "s"] "a"
-- Or, RHS as a function?

rhs2 :: StreamGraph -> StreamGraph
rhs2 (Connect (Vertex (StreamVertex i Filter (f1:_) intype))
              (Vertex (StreamVertex _ Filter (f2:_) _))) =
    Vertex $ StreamVertex i Filter ["\\f g x -> f x && g x", f1, f2, "s"] intype
    -- XXX perhaps, we could use pattern matching to both chekc *and* apply rewrites,
    -- with a catch-all pattern for "no match, no op"
rhs2 g = g

{- 

    the vertex id is superfluous.
    the stream type is probably superfluous (could a rewrite rule
    change the type of the stream? most likely not.)
    the variables embedded in the LHS are buried in a string in the
    RHS

    next step: how would we go about matching the lhs against an
    instance of streamgraph and seeing if it applies?

    ...the above doesn't even need StreamVertex to be based in terms
    of an abstract type
-}

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
