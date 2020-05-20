{-# LANGUAGE TemplateHaskell #-}
{-
 - Striot StreamGraph type, used for representing a stream processing program,
 - such that it can be re-written and partitioned.
 -}

module Striot.StreamGraph ( StreamGraph(..)
                          , StreamOperator(..)
                          , StreamVertex(..)
                          , showParam
                          ) where

import Algebra.Graph
import Data.List (intercalate)
import Language.Haskell.TH
import System.IO.Unsafe (unsafePerformIO)
import Test.Framework -- Arbitrary, etc.

-- |The `StreamOperator` and associated information required to encode a stream-processing
-- program into a Graph. Each distinct `StreamVertex` within a `StreamGraph` should have a
-- unique `vertexId` to ensure that they can be distinguished. For simple path-style graphs,
-- the IDs should be in ascending order.
data StreamVertex = StreamVertex
    { vertexId   :: Int
    , operator   :: StreamOperator
    , parameters :: [ExpQ]
    , intype     :: String
    , outtype    :: String
    }

-- omitting comparing parameters
instance Eq StreamVertex where
    a == b = and [ vertexId a == vertexId b
                 , operator a == operator b
                 , intype a   == intype b
                 , outtype a  == outtype b
                 ]

-- this is needed for generating GraphViz output, and also
-- is a requirement of ???
instance Show StreamVertex where
    show (StreamVertex i o ps inT outT) =
        "StreamVertex " ++ intercalate " "
            [ show i
            , show o
            , "[?]"
            , show inT
            , show outT
            ]

deQ :: Q Exp -> Exp
deQ = unsafePerformIO . runQ

showParam :: Q Exp -> String
showParam qexp = pprint (deQ qexp)

-- |A graph representation of a stream-processing program.
type StreamGraph = Graph StreamVertex

-- |An enumeration of the possible stream operators within a stream-processing program,
-- as well as `Source` and `Sink` to represent the ingress and egress points of programs.
data StreamOperator = Map
                    | Filter
                    | Expand
                    | Window
                    | Merge
                    | Join
                    | Scan
                    | FilterAcc
                    | Source
                    | Sink
                    deriving (Show,Ord,Eq)

instance Ord StreamVertex where
    compare x y = compare (vertexId x) (vertexId y)

------------------------------------------------------------------------------
-- quickcheck experiment

instance Arbitrary StreamOperator where
    arbitrary = elements [ Map , Filter , Expand , Window , Merge , Join , Scan
                         , FilterAcc , Source , Sink ]

instance Arbitrary StreamVertex where
    arbitrary = do
        vertexId <- arbitrary
        operator <- arbitrary
        let parameters = []
            ty = "String" in
            return $ StreamVertex vertexId operator parameters ty ty

streamgraph :: Gen StreamGraph
streamgraph = sized streamgraph'
streamgraph' 0 = return g where g = empty :: StreamGraph
streamgraph' n | n>0 = do
    v <- arbitrary
    t <- streamgraph' (n-1)
    return $ connect (vertex v) t
