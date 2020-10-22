{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}

module Striot.VizGraph ( streamGraphToDot
                       , displayGraph
                       , displayGraphKitty
                       , htf_thisModulesTests) where

import Striot.StreamGraph
import Algebra.Graph
import Algebra.Graph.Export.Dot
import Data.String
import Test.Framework
import Data.List (intercalate)
import Data.List.Split
import Language.Haskell.TH

import System.Process
import System.IO (openTempFile, hPutStr, hGetContents, hClose)

streamGraphToDot :: StreamGraph -> String
streamGraphToDot = export myStyle

show' :: StreamVertex -> String
show' v = intercalate " " $ ((printOp . operator) v) : ((map (\s->"("++s++")")) . map showParam . parameters) v

printOp :: StreamOperator -> String
printOp = (++) "stream" . show

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

test_escape_1 = assertEqual "no escaping"        $ escape ("no escaping")
test_escape_2 = assertEqual "escaped \\\" quote" $ escape ("escaped \" quote")
test_escape_3 = assertEqual "escaped \\\\ backslash" $ escape ("escaped \\ backslash")

-- test data

source x = [| do
    let x' = $(litE (StringL x))
    threadDelay (1000*1000)
    putStrLn "sending '"++x'++"'"
    return x'
    |]

v1 = StreamVertex 1 Source [source "foo"]    "String" "String"
v2 = StreamVertex 2 Map    [[| id |]]        "String" "String"
v3 = StreamVertex 3 Source [source "bar"]    "String" "String"
v4 = StreamVertex 4 Map    [[| id |]]        "String" "String"
v5 = StreamVertex 5 Merge  []                "[String]" "String"
v6 = StreamVertex 6 Sink   [[| mapM_ print|]] "String" "IO ()"
mergeEx :: StreamGraph
mergeEx = overlay (path [v3, v4, v5]) (path [v1, v2, v5, v6])

v7 = StreamVertex  1 Source [[| sourceOfRandomTweets |]] "String" "String"
v8 = StreamVertex  2 Map    [[| filter (('#'==).head) . words |]] "String" "[String]"
v9 = StreamVertex  5 Expand [] "[String]" "String"
v10 = StreamVertex 6 Sink   [[|mapM_ print|]] "String" "IO ()"
expandEx :: StreamGraph
expandEx = path [v7, v8, v9, v10]

displayGraph :: StreamGraph -> IO ()
displayGraph g = do
    (Just hin,Just hout,_, _) <- createProcess (proc "dot" ["-Tpng"])
      { std_out = CreatePipe, std_in = CreatePipe }
    _ <- createProcess (proc "display" []){ std_in = UseHandle hout }

    hPutStr hin (streamGraphToDot g)
    hClose hin

displayGraphKitty :: StreamGraph -> IO ()
displayGraphKitty g = do
    (Just hin, Just hout, _, _)   <- createProcess (proc "dot" ["-Tpng"])
      { std_out = CreatePipe, std_in = CreatePipe }
    (_, Just hout2, _, _) <- createProcess (proc "base64" ["-w0"])
      { std_in = UseHandle hout , std_out = CreatePipe }

    hPutStr hin (streamGraphToDot g)
    hClose hin
    foo <- hGetContents hout2
    let bar = chunksOf 4096 foo
    mapM_ (\c -> putStr $ "\ESC_Gf=100,a=T,m=1;" ++ c ++ "\ESC\\") (init bar)
    putStrLn $ "\ESC_Gf=100,a=T,m=0;" ++ (last bar) ++ "\ESC\\"
