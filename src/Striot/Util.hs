module Striot.Util ( toDot ) where

import Algebra.Graph

toDot :: Ord a => Show a => Graph a -> String
toDot g = "digraph {\n" ++ vertexDefs ++ (toDot' (edgeList g)) ++ "}\n" where
    toDot' [] = ""
    toDot' (e:es) = (blah e) ++ (toDot' es)
    blah (v1,v2) = "\t\"" ++ (show' v1) ++ "\" -> \"" ++ (show' v2) ++ "\";\n"
    vertexDefs = concatMap (\x -> "\t\"" ++ (show' x) ++ "\" [label=\"" ++ (show' x) ++ "\"]" ++ ";\n") (vertexList g)
    show' = escape . show

escape :: String -> String
escape [] = []
escape (s:ss) = if   s `elem` escapeme
                then '\\':s:(escape ss)
                else s:(escape ss)
    where
        escapeme = "\\\":"
    --safechars = concat [['a'..'z'],['A'..'Z'],['\200'..'\377'],"_"]
