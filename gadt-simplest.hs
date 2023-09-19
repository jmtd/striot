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


data StreamProg = StreamProg StreamOperator [Exp] String String ServiceTime [StreamProg]
                 deriving (Show, Eq)


-- what StreamGraph will demonstrate the issue of vertexIds getting reshuffled?
sample1 = simpleStream
  [ ((Source 1) , [[| sourceFn |]], "Int", 0)
  , ((Filter 0.5), [[| (>5) |]], "Int", 1)
  , ((Filter 0.5), [[| (<8) |]], "Int", 1)
  , (Window , [[| chop 1 |]], "[Int]", 1)
  , (Sink   , [sinkFn], "[String]", 0)
  ]
v = head $ vertexList sample1

sample1d = StreamProg (Source 1)    [deQ [| sourceFn |]] "Int" "Int" 0 []
         & StreamProg (Filter 0.5) [deQ [| (>5) |]]     "Int"   "Int"      1 .(:[])
         & StreamProg (Filter 0.5) [deQ [| (<8) |]]     "Int"   "Int"      1 .(:[])
         & StreamProg Window       [deQ [| chop 1 |]]   "Int"   "[Int]"    1 .(:[])
         & StreamProg Sink         [deQ sinkFn]         "[Int]" "[String]" 0 .(:[])

-- construct a partially-applied StreamProg from a StreamVertex; lacking the
-- final parent StreamProg parameter. The vertexID is not preserved.
fromStreamVertex :: StreamVertex -> ([StreamProg] -> StreamProg)
fromStreamVertex (StreamVertex _ o p i ot s) = StreamProg o (map deQ p) i ot s

-- | construct a partial StreamVertex from a StreamProg node. Return a function
-- that requires the final vertexId.
toStreamVertex :: StreamProg -> (Int -> StreamVertex)
toStreamVertex (StreamProg o p i ot st _) = \v ->
    StreamVertex v o (map return p) i ot st

prop_tofromStreamVertex_idem sv = sv == toStreamVertex (fromStreamVertex sv []) (vertexId sv)

fromStreamGraph :: StreamGraph -> StreamProg
fromStreamGraph sg = let
    sink = (snd . head . filter ((==) Sink . operator . snd) . edgeList) sg
    in fromStreamGraph' sg sink

fromStreamGraph' :: StreamGraph -> StreamVertex -> StreamProg
fromStreamGraph' sg v = let
    incoming = (map fst . filter ((==) v . snd) . edgeList) sg
    in (fromStreamVertex v) $ map (fromStreamGraph' sg) incoming

-- toStreamGraph :: StreamProg -> StreamGraph
-- toStreamGraph (StreamProg o p i ot st)
