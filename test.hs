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
rhs = StreamVertex 0 Filter ["(\\x -> f x && g x)", "s"] "a"

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

--doesRuleMatch :: Graph StreamVertex -> Graph StreamVertex -> Bool
--doesRuleMatch g lhs = let
--    -- find the entrance node (nodes?) into the lhs

entryNodes :: Graph a -> [a]
entryNodes Empty = []
entryNodes (Overlay f g) = (entryNodes f) ++ (entryNodes g)
entryNodes (Connect (Vertex v) g) = [v]
entryNodes (Connect f g) = entryNodes f -- all nodes in g have incoming edges so can't be entry nodes.
entryNodes (Vertex v) = []

-- this doesn't work at all, yet. it identifies candidate nodes, but they might have inbound
-- edges from other parts of the expression (e.g. exampleStream above has each filter represented
-- twice:
--      streamSource * streamFilter + streamFilter * streamFilter + streamFilter * streamSink
--)

entryNodes' g = let
    vl = vertexList g
    el = edgeList g
    sources = map fst el
    sinks = map snd el
    in filter (\v -> v `elem` sources && not (v `elem` sinks)) vl
    -- XXX first part of filter probably unnecessary
