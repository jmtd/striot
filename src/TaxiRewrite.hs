-- Jon's attempt to use this stuff for stream rewrites
-- temp hacky copy to explore possible rewrites for Taxi

module MyMain where

import Expressions
import Parsing
import Laws
import Calculations

import Control.Monad (liftM2)
import Data.List (permutations,nub)


-- hack to encapsulate (\f g x -> f x && g x)
-- XXX more descriptive name
zomg f g x = f x && g x

-- hacks to encapsulate filterAcc fusion components
fusedAcc  accfn1 accfn2 pred1         (x,y) v = (accfn1 x v, if pred1 v x then accfn2 y v else y)
fusedPred               pred1 pred2   x (y,z) = pred1 x y && pred2 x z
comma = (,)

-- hacks for filter/filterAcc fusion
fAcc  p f  = \a v -> if p v then f a v else a
fPred p q  = \v a -> p v && q v a
fPred2 p q = \v a -> p v a && q v

-- XXX: encode the remaining transformations in particular streamJoin and streamFilterAcc transformations
mylaws = map (parse law) [ {-1-} "defn filterfuse: streamFilter f . streamFilter g = streamFilter (zomg f g)"
                         , {-5-} "defn mapFilter:  streamFilter p . streamMap f = streamMap f . streamFilter (p . f)"
                         , {-6-} "defn mapfuse:    streamMap f . streamMap g = streamMap (f . g)"
                         , {-7-} "defn mapWindow:  streamWindow w . streamMap f = streamMap (map f) . streamWindow w"

                         {-  8 join, skipping -}
                         {-  9-12 expand, skipping -}
                         {-  13,14 merge, skipping -}

                         {- 15-18 inversion caveat, skipping -}
                         {- 19,20, expand, skipping -}
                         {- 21,22 merge, skipping --}
                         {- 23-26 merge/expand and inverses, skipping -}

                         , "defn mapFilterAcc: \
\                           streamFilterAcc g a p . streamMap f \
\                           = streamMap f . streamFilterAcc g a (p . f)"

                         , "defn filterFilterAcc: \
\                           streamFilterAcc f a q . streamFilter p \
\                           = streamFilterAcc (fAcc p f) a (fPred p q)"

                         , "defn filterAccFilter: \
\                           streamFilter q . streamFilterAcc f a p \
\                           = streamFilterAcc f a (fPred2 p q)"

                         , "defn filterAccfilterAcc: \
\                          streamFilterAcc f2 a2 p2 . streamFilterAcc f1 a1 p1\
\                        = streamFilterAcc (fusedAcc f1 f2 p1) (comma a1 a2) (fusedPred p1 p2)"

                         , "defn FilterAccMap: streamFilterAcc f2 a p . streamMap f = streamMap f . streamFilterAcc f2 a (p . f)"
                         ]

taxiQ1 = parse expr $ concat [ "streamFilterAcc a1 a p2" -- journeyChanges
                             , " . streamMap f"               -- topk
                             , " . streamWindow w"            -- slidingTime 1800 (part of: streamWindowAggregate)
                             , " . streamFilter p"            -- inRange && inRange
                             , " . streamMap f2"              -- tripToJourney
                             ]

foo7 = calculate mylaws taxiQ1

-- decomposed representation of the streamgraph for taxiQ2. The representation of the duplicate streams into
-- the join is troublesome
taxiQ2 = parse expr "streamFilterAcc a b p1 . streamMap f1 . streamWindow w1 \
                        \ . streamMap f2 . streamJoin \
                                            \ (streamFilter p2 . streamMap f3) \
                                            \ (streamFilter p2 . streamMap f3)"

foo8 = calculate mylaws taxiQ2

-- test filterAcc fusion
foo9 = calculate mylaws (parse expr "streamFilterAcc a b p1 . streamFilterAcc c d p2")
fooA = calculate mylaws (parse expr "streamFilterAcc a b p1 . streamFilter p2")

-- next step: generate every permutation of laws, then use each with calculate,
-- then pull out the ultimate exprs, then reduce to unique versions, how many do
-- we have?
-- (\(Calc e ss) -> snd $ ss !! 3) foo7 :: Expr (implements Eq)

-- this is slow, but eventually we end up with [(\ (Calc e ss) -> (snd . last) ss) foo7]
foosB = nub
      $ map (\laws -> calculate laws taxiQ1)
        (permutations mylaws)
      -- $ map (\ (Calc e ss) -> (snd . last) ss)
