{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-
 - keep it simple: a type that mirrors StreamGraph as close as
 - possible whilst encoding the graph structure and not using
 - vertexIds
 - -}

import Algebra.Graph
import Striot.StreamGraph
import Data.Function ((&))
import Language.Haskell.TH -- ExpQ
import Test.Framework

type ServiceTime = Double
type ArrivalRate = Double

sinkFn = [| mapM_ $ putStrLn . ("receiving "++) . show . value |] :: ExpQ

data StreamProg = StreamProg Int StreamOperator [Exp] String String ServiceTime [StreamProg]
    deriving (Show)

-- deliberately ignore the vertexId element. If a rewrite rule causes vertex
-- renumbering, but the structure is identical and the expressions the same,
-- we consider them equal.
instance Eq StreamProg where
    (StreamProg id1 op1 exps1 int1 out1 st1 parents1) ==
        (StreamProg id2 op2 exps2 int2 out2 st2 parents2) = and
            [ op1      == op2
            , exps1    == exps2
            , int1     == int2
            , out1     == out2
            , st1      == st2
            , parents1 == parents2

sample1 = simpleStream
  , ((Filter 0.5), [[| (>5) |]], "Int", 1)
  , ((Filter 0.5), [[| (<8) |]], "Int", 1)
  , (Window , [[| chop 1 |]], "[Int]", 1)
  , (Sink   , [sinkFn], "[String]", 0)
  ]
v = head $ vertexList sample1

sample1d = StreamProg 1 (Source 1)   [deQ [| sourceFn |]] "IO ()" "Int"      0 []
         & StreamProg 2 (Filter 0.5) [deQ [| (>5) |]]     "Int"   "Int"      1 .(:[])
         & StreamProg 3 (Filter 0.5) [deQ [| (<8) |]]     "Int"   "Int"      1 .(:[])
         & StreamProg 4 Window       [deQ [| chop 1 |]]   "Int"   "[Int]"    1 .(:[])
         & StreamProg 5 Sink         [deQ sinkFn]         "[Int]" "[String]" 0 .(:[])

-- construct a partially-applied StreamProg from a StreamVertex; lacking the
-- final parent StreamProg parameter.
fromStreamVertex :: StreamVertex -> ([StreamProg] -> StreamProg)
fromStreamVertex (StreamVertex v o p i ot s) = StreamProg v o (map deQ p) i ot s

toStreamVertex :: StreamProg -> StreamVertex
toStreamVertex (StreamProg v o p i ot st _) = StreamVertex v o (map return p) i ot st

prop_tofromStreamVertex_idem sv = sv == toStreamVertex (fromStreamVertex sv [])

fromStreamGraph :: StreamGraph -> StreamProg
fromStreamGraph sg = let
    sink = (snd . head . filter ((==) Sink . operator . snd) . edgeList) sg
    in fromStreamGraph' sg sink

fromStreamGraph' :: StreamGraph -> StreamVertex -> StreamProg
fromStreamGraph' sg v = let
    incoming = (map fst . filter ((==) v . snd) . edgeList) sg
    in fromStreamVertex v $ map (fromStreamGraph' sg) incoming

toStreamGraph :: StreamProg -> StreamGraph
toStreamGraph sp@(StreamProg _ _ _ _ _ _ par) = let
    v = toStreamVertex sp
    in edges (map (\p -> (toStreamVertex p, v)) par)
        `overlay` overlays (map toStreamGraph par)

-- guard for QuickCheck properties
haveEdgesToSink :: StreamGraph -> Bool
haveEdgesToSink = not . null . filter ((==) Sink . operator . snd) . edgeList

prop_tofromStreamGraph_idem sg = 
    haveEdgesToSink sg ==> sg == toStreamGraph (fromStreamGraph sg)
