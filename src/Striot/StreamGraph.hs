{-# LANGUAGE TemplateHaskell #-}
{-
 - Striot StreamGraph type, used for representing a stream processing program,
 - such that it can be re-written and partitioned.
 -}

module Striot.StreamGraph ( StreamGraph(..)
                          , StreamGraphQ(..)
                          , StreamOperator(..)
                          , StreamVertex(..)
                          , StreamVertexQ(..)
                          , deQ
                          ) where

import Algebra.Graph
import Language.Haskell.TH
import Test.Framework -- Arbitrary, etc.

deQ :: Graph StreamVertexQ -> IO StreamGraph
deQ = foldg
    (return empty)
    runVertex
 -- (\ x y -> x >>= \x' -> y >>= return . overlay x')
    (\ x y -> x >>= \x' -> y >>= return . overlay x')
 -- (\ x -> \ y -> x >>= \x' -> y >>= return . overlay x')
 -- (\ x -> \ y -> (=<<) (\x' -> y >>= return . overlay x') x)
 -- (\ y -> (=<<) (\x' -> y >>= return . overlay x'))
    (\ x y -> do
        x' <- x
        y' <- y
        return (connect x' y')
    )

runVertex :: StreamVertexQ -> IO StreamGraph
runVertex (StreamVertexQ i ops qpars inT outT) = do
    pars <- mapM runQ qpars
    return $ Vertex (StreamVertex i ops pars inT outT)

-- |The `StreamOperator` and associated information required to encode a stream-processing
-- program into a Graph. Each distinct `StreamVertex` within a `StreamGraph` should have a
-- unique `vertexId` to ensure that they can be distinguished. For simple path-style graphs,
-- the IDs should be in ascending order.
data StreamVertex = StreamVertex
    { vertexId   :: Int
    , operator   :: StreamOperator
    , parameters :: [Exp]
    , intype     :: String
    , outtype    :: String
    } deriving (Eq,Show)

data StreamVertexQ = StreamVertexQ
    { vertexIdQ  :: Int
    , operatorQ  :: StreamOperator
    , parametersQ:: [ExpQ]
    , intypeQ    :: String
    , outtypeQ   :: String
    }

-- |A graph representation of a stream-processing program.
type StreamGraph = Graph StreamVertex
type StreamGraphQ = Graph StreamVertexQ

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
