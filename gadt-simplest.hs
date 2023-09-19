{-
 - keep it simple: a type that mirrors StreamGraph as close as
 - possible whilst encoding the graph structure and not using
 - vertexIds
 - -}

import Striot.StreamGraph
import Data.Function ((&))
import Language.Haskell.TH -- ExpQ

type ServiceTime = Double

{-
data StreamProg   = StreamProg
  { operator   :: StreamOperator
  , parameters :: [ExpQ]
  , intype     :: String
  , outtype    :: String
  , serviceTime:: ServiceTime
  , parent     :: Maybe StreamProg -- not for streamSrc
  }

-- what StreamGraph will demonstrate the issue of vertexIds getting reshuffled?
sample1 = simpleStream
  [ ((Source 1) , [[| sourceFn |]], "Int", 0)
  , ((Filter 0.5), [[| (>5) |]], "Int", 1)
  , ((Filter 0.5), [[| (<8) |]], "Int", 1)
  , (Window , [[| chop 1 |]], "[Int]", 1)
  , (Sink   , [sinkFn], "[String]", 0)
  ]

-- an equivalent?
sample1b = StreamProg Sink [sinkFn] "[Int]" "[String]" 0 $
  Just $ StreamProg Window       [[| chop 1 |]]   "Int" "[Int]" 1 $
  Just $ StreamProg (Filter 0.5) [[| (<8) |]]      "Int" "Int" 1 $
  Just $ StreamProg (Filter 0.5) [[| (>5) |]]      "Int" "Int" 1 $
  Just $ StreamProg (Source 1)   [[| sourceFn |]] "Int" "Int" 0 Nothing

sample1c = StreamProg (Source 1)   [[| sourceFn |]] "Int"   "Int"      0 Nothing
         & StreamProg (Filter 0.5) [[| (>5) |]]     "Int"   "Int"      1 . Just
         & StreamProg (Filter 0.5) [[| (<8) |]]     "Int"   "Int"      1 . Just
         & StreamProg Window       [[| chop 1 |]]   "Int"   "[Int]"    1 . Just
         & StreamProg Sink         [sinkFn]         "[Int]" "[String]" 0 . Just
-}
------------------------------------------------------------------------------
sinkFn = [| mapM_ $ putStrLn . ("receiving "++) . show . value |] :: ExpQ

data StreamProg2   = StreamProg2
  { operator   :: StreamOperator
  , parameters :: [Exp]
  , intype     :: String
  , outtype    :: String
  , serviceTime:: ServiceTime
  , parents    :: [StreamProg2]
  } | StreamSource 
  { arrivalRate:: Double
  , srcParams  :: [Exp]
  , srcInType  :: String
  , srcOutType :: String
  , srcSvcTime :: ServiceTime
  } deriving Eq

sample1d = StreamSource 1           [deQ [| sourceFn |]] "Int" "Int" 0
         & StreamProg2 (Filter 0.5) [deQ [| (>5) |]]     "Int"   "Int"      1 .(:[])
         & StreamProg2 (Filter 0.5) [deQ [| (<8) |]]     "Int"   "Int"      1 .(:[])
         & StreamProg2 Window       [deQ [| chop 1 |]]   "Int"   "[Int]"    1 .(:[])
         & StreamProg2 Sink         [deQ sinkFn]         "[Int]" "[String]" 0 .(:[])
