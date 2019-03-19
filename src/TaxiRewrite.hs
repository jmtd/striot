-- Richard Bird's example "pointless calculator" adapted as PoC for
-- stream graph rewriting

module MyMain where

import Expressions
import Parsing
import Laws
import Calculations

import Control.Monad (liftM2)
import Data.List (permutations,nub)

-- hacks to encode parts of rewrite rules that are not directly expressable in
-- the restricted syntax supported by this calculator's parser
filterFuseHelper f g x = f x && g x
fusedAcc  accfn1 accfn2 pred1         (x,y) v = (accfn1 x v, if pred1 v x then accfn2 y v else y)
fusedPred               pred1 pred2   x (y,z) = pred1 x y && pred2 x z
fAcc  p f  = \a v -> if p v then f a v else a
fPred p q  = \v a -> p v && q v a
fPred2 p q = \v a -> p v a && q v

mylaws = map (parse law)
 [ "defn filterfuse: streamFilter f . streamFilter g = streamFilter (filterFuseHelper f g)"
 , "defn mapFilter:  streamFilter p . streamMap f = streamMap f . streamFilter (p . f)"
 , "defn mapfuse:    streamMap f . streamMap g = streamMap (f . g)"
 , "defn mapWindow:  streamWindow w . streamMap f = streamMap (map f) . streamWindow w"

 , "defn mapFilterAcc: \
\   streamFilterAcc g a p . streamMap f \
\   = streamMap f . streamFilterAcc g a (p . f)"

 , "defn filterFilterAcc: \
\   streamFilterAcc f a q . streamFilter p \
\   = streamFilterAcc (fAcc p f) a (fPred p q)"

 , "defn filterAccFilter: \
\   streamFilter q . streamFilterAcc f a p \
\   = streamFilterAcc f a (fPred2 p q)"

 , "defn filterAccfilterAcc: \
\   streamFilterAcc f2 a2 p2 . streamFilterAcc f1 a1 p1\
\   = streamFilterAcc (fusedAcc f1 f2 p1) (a1 , a2) (fusedPred p1 p2)"

 , "defn FilterAccMap: streamFilterAcc f2 a p . streamMap f = streamMap f . streamFilterAcc f2 a (p . f)"
 ]

taxiQ1 = parse expr $ concat
 [ "streamFilterAcc a1 a p3" -- journeyChanges
 , " . streamMap f"          -- topk
 , " . streamWindow w"       -- slidingTime 1800
 , " . streamFilter p2"      -- inRange (end)
 , " . streamFilter p1"      -- inRange (start)
 , " . streamMap f2"         -- tripToJourney
 ]

onePerm  = calculate mylaws taxiQ1

allPerms = nub
         $ map (\laws -> calculate laws taxiQ1)
           (permutations mylaws)

testFilterAccFuse = parse expr "streamFilterAcc f2 a2 p2 . streamFilterAcc f1 a1 p1"
testFilterAccFuse'= calculate mylaws stupid

