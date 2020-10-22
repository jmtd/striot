{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}

module Striot.VizGraph ( streamGraphToDot
                       , displayGraph
                       , htf_thisModulesTests) where

import Striot.StreamGraph
import Algebra.Graph
import Algebra.Graph.Export.Dot
import Data.String
import Test.Framework
import Data.List (intercalate)
import Language.Haskell.TH

import System.Process
import System.IO (openTempFile, hPutStr, hClose)

streamGraphToDot :: StreamGraph -> String
streamGraphToDot = export myStyle

show' :: StreamVertex -> String
show' v = intercalate " " $ ((printOp . operator) v) : ((map (\s->"("++s++")")) . map showParam . parameters) v

printOp :: StreamOperator -> String
printOp (Filter _)        = "streamFilter"
printOp (FilterAcc _)     = "streamFilterAcc"
printOp (Source _)        = "streamSource"
printOp x                 = "stream" ++ (show x)

myStyle :: Style StreamVertex String
myStyle = Style
    { graphName               = mempty
    , preamble                = mempty
    , graphAttributes         = ["bgcolor":="white"]
    , defaultVertexAttributes = ["shape" := "box","fillcolor":="white","style":="filled"]
    , defaultEdgeAttributes   = ["weight":="10","color":="black","fontcolor":="black"]
    , vertexName              = show . vertexId
    , vertexAttributes        = (\v -> ["label":=(escape . show') v])
    , edgeAttributes          = (\_ o -> ["label":=intype o])
    }

-- escape a string, suitable for inclusion in a .dot file
escape [] = []
escape (x:xs) = case x of
    '"'  -> '\\':'"' : escape xs
    '\\' -> '\\':'\\': escape xs
    _    -> x        : escape xs

-- test data

source x = [| do
    let x' = $(litE (StringL x))
    threadDelay (1000*1000)
    putStrLn "sending '"++x'++"'"
    return x'
    |]

v1 = StreamVertex 1 (Source 1) [source "foo"]    "String" "String" 0
v2 = StreamVertex 2 Map    [[| id |]]        "String" "String" 1
v3 = StreamVertex 3 (Source 1) [source "bar"]    "String" "String" 2
v4 = StreamVertex 4 Map    [[| id |]]        "String" "String" 3
v5 = StreamVertex 5 Merge  []                "[String]" "String" 4
v6 = StreamVertex 6 Sink   [[| mapM_ print|]] "String" "IO ()" 5
mergeEx :: StreamGraph
mergeEx = overlay (path [v3, v4, v5]) (path [v1, v2, v5, v6])

v7 = StreamVertex  1 (Source 1) [[| sourceOfRandomTweets |]] "String" "String" 0
v8 = StreamVertex  2 Map    [[| filter (('#'==).head) . words |]] "String" "[String]" 1
v9 = StreamVertex  5 Expand [] "[String]" "String" 2
v10 = StreamVertex 6 Sink   [[|mapM_ print|]] "String" "IO ()" 3
expandEx :: StreamGraph
expandEx = path [v7, v8, v9, v10]

displayGraph :: StreamGraph -> IO ()
displayGraph g = do
    (Just hin,Just hout,_, _) <- createProcess (proc "dot" ["-Tpng"])
      { std_out = CreatePipe, std_in = CreatePipe }
    _ <- createProcess (proc "display" []){ std_in = UseHandle hout }

    hPutStr hin (streamGraphToDot g)
    hClose hin
