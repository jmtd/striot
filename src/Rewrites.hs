{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Rewrites (htf_thisModulesTests) where

import Data.List (sort)
import Data.Char (isAscii)
import Test.Framework
import Striot.FunctionalIoTtypes
import Striot.FunctionalProcessing

{-
    possible StreamGraph rewrites, via combination of operators
 -}

main = htfMain htf_thisModulesTests

-------------------------------------------------------------------------------
-- utility functions

jClean = map (\(Event _ _ (Just v)) -> v)

sA = [Event 0 Nothing (Just i)|i<-iterate next 'a']
sB = [Event 0 Nothing (Just i)|i<-iterate next '0']
sC = [Event 0 Nothing (Just i)|i<-iterate next 'A']
sW = streamWindow (chop 2) sB

-- filter predicates
f = (>= 'a')
g = (<= 'z')

-- avoids a situation where pred/succ will fail on the smallest/largest Enum type
next :: (Eq a, Bounded a, Enum a) => a -> a
next a = if a == maxBound then minBound else succ a
prev :: (Eq a, Bounded a, Enum a) => a -> a
prev a = if a == minBound then maxBound else pred a

-------------------------------------------------------------------------------
-- streamFilter → streamFilter
-- 1

filterFilterPre     = streamFilter g . streamFilter f
filterFilterPost    = streamFilter (\x -> f x && g x)
prop_filterFilter s = filterFilterPre s == filterFilterPost s

-------------------------------------------------------------------------------
-- streamFilter → streamFilterAcc
-- 2

filterFilterAccPre = streamFilterAcc accfn1 acc1 pred1 . streamFilter g

-- promote a streamFilter into a streamFilterAcc
-- the accumulator type is Char but this shouldn't matter
promote :: (a -> Bool) -> Stream a -> Stream a
promote pred = streamFilterAcc (\a _ -> a) '0' (\v _ -> pred v)

prop_promotedFilter s = streamFilter g s == promote g s

filterFilterAccPost    = mkfAccFuse (\x v -> x) '0' (\v _ -> g v) accfn1 acc1 pred1
prop_filterFilterAcc s = filterFilterAccPre s == filterFilterAccPost s

-- this is to demonstrate that the type of the dummy accumulator for the promoted
-- streamFilter is unimportant and does not need to match the type of the other
-- accumulator
filterFilterAccPost2    = mkfAccFuse (\x v -> x) (0::Int) (\v _ -> g v) accfn1 acc1 pred1
prop_filterFilterAcc2 s = filterFilterAccPre s == filterFilterAccPost2 s

-- unwinding mkfAccFuse, for  convenience in the writeup
filterFilterAccPost3    = streamFilterAcc
    (\a v -> if g v then accfn1 a v else a)
    acc1
    (\x a -> g x && pred1 x a)

prop_filterFilterAcc3 s = filterFilterAccPre s == filterFilterAccPost3 s

-------------------------------------------------------------------------------
-- streamFilter → streamMap

-- None:
-- We would need the inverse of the map function

-------------------------------------------------------------------------------
-- streamFilter → streamScan

-- None:
-- As above.

-------------------------------------------------------------------------------
-- streamFilter → streamWindow

-- Nothing
-- Rationale: the filter is operating on some atomic a, we don't know the window
-- logic so we don't know if it can properly handle the rejected events, and what
-- 'shape' the windows should be if we tried to map the filter into them afterwards

-- streamFilter → streamExpand
-- Nothing
-- Rationale: filter must be operating on a list type [a], and rejecting or accepting
-- a full list of things. if we expand those things into a single stream, we lose the
-- grouping information and so can't derive a filter that operated identically

------------------------------------------------------------------------------
-- streamFilter → streamMerge

filterMergePre  s = streamMerge [streamFilter f sA, streamFilter f s]
filterMergePost s = streamFilter f $ streamMerge [sA, s]

-- false! this only works if ordering is not important, or if we can re-order
-- post merge (sort on a timestamp?)

-- reordering is externally dependent on the streamFilter predicate
-- XXX 

txxt_filterMerge = assertBool $ sort (take 10 (filterMergePre sB))
                             == sort (take 10 (filterMergePost sB))
-- these are also very slow to execute
pxxp_filterMerge s = sort (filterMergePre s) == sort (filterMergePost s)

------------------------------------------------------------------------------
-- streamFilter → streamJoin

-- Nothing
-- TODO explain

------------------------------------------------------------------------------
-- streamFilterAcc → streamFilter
-- see the inverse for details
-- 3

filterAccFilterPre     = streamFilter g . streamFilterAcc accfn1 acc1 pred1
filterAccFilterPost    = mkfAccFuse accfn1 acc1 pred1 (\_ v -> v) '0' (\v _ -> g v)
prop_filterAccFilter s = filterFilterAccPre s == filterFilterAccPost s


filterAccFilterPost2
         = streamFilterAcc accfn1 acc1 (\x a -> pred1 x a && g x)

prop_filterAccFilter2 s = filterAccFilterPre s == filterAccFilterPost2 s
------------------------------------------------------------------------------
-- streamFilterAcc → streamFilterAcc
-- 4

-- alternating values only
accfn2 _ v = v
acc2 = '\NUL'
pred2 = (/=)

-- increasing values only
accfn1 _ v = v
acc1 = '\NUL'
pred1 = (>=)

-- build a fused filterAcc function
-- argument order assumes data flow left-to-right, i.e. the opposite way to
-- function composition
-- XXX test that b and c can be distinct types
mkfAccFuse :: (b -> a -> b) -> b -> (a -> b -> Bool)
           -> (c -> a -> c) -> c -> (a -> c -> Bool)
           -> Stream a -> Stream a
mkfAccFuse accfn1 acc1 pred1 accfn2 acc2 pred2 = streamFilterAcc
    (\(x,y) v -> (accfn1 x v, if pred1 v x then accfn2 y v else y))
    (acc1,acc2)
    (\x (y,z) -> pred1 x y && pred2 x z)

fAccfAccPre      = streamFilterAcc accfn2 acc2 pred2 . streamFilterAcc accfn1 acc1 pred1
fAccfAccPost     = mkfAccFuse accfn1 acc1 pred1 accfn2 acc2 pred2
prop_fAccfAcc1 s = fAccfAccPre s == fAccfAccPost s

-- reverse order
fAccfAccPre2     = streamFilterAcc accfn1 acc1 pred1 . streamFilterAcc accfn2 acc2 pred2
fAccfAccPost2    = mkfAccFuse accfn2 acc2 pred2 accfn1 acc1 pred1
prop_fAccfAcc2 s = fAccfAccPre2 s == fAccfAccPost2 s

-------------------------------------------------------------------------------
-- streamFilterAcc → streamMap

-- Nothing -- see discussion in streamFilter → streamMap

-------------------------------------------------------------------------------
-- streamFilterAcc → streamScan

-- Nothing -- see discussion in streamFilter → streamMap

-------------------------------------------------------------------------------
-- streamFilterAcc → streamWindow
-- streamFilterAcc → streamExpand

-- Nothing . See streamFilter → stream{Expand,Window}

-------------------------------------------------------------------------------
-- streamFilterAcc → streamMerge

-- TODO

-------------------------------------------------------------------------------
-- streamFilterAcc → streamJoin

-- TODO

------------------------------------------------------------------------------
-- streamMap → streamFilter
-- streamFilter p . streamMap f = streamMap f . streamFilter (p . f)
-- some kind of trade off depending on how selective p is?
-- 5

mff = succ

mapFilterPre     = streamFilter f . streamMap mff
mapFilterPost    = streamMap mff . streamFilter (f . mff)
prop_mapFilter s = mapFilterPre s == mapFilterPost s

------------------------------------------------------------------------------
-- streamMap → streamFilterAcc
-- streamFilterAcc accfn acc p . streamMap f = streamMap f . streamFilterAcc accfn acc (p . f)

accfn acc _ = acc+1
accpred dat acc = even acc

mapFilterAccPre :: Stream Char -> Stream Char
mapFilterAccPre = streamFilterAcc accfn 0 accpred . streamMap next

mapFilterAccPost :: Stream Char -> Stream Char
mapFilterAccPost = streamMap next
    . streamFilterAcc accfn 0 (accpred . next)

prop_mapFilterAcc s = mapFilterAccPre s == mapFilterAccPost s

------------------------------------------------------------------------------
-- streamMap → streamMap
-- fuse
-- 6

mapMapPre :: Stream Char -> Stream Char
mapMapPre     = streamMap succ . streamMap succ
mapMapPost    = streamMap (succ . succ)
prop_mapMap s = mapMapPre s == mapMapPost s

------------------------------------------------------------------------------
-- streamMap → streamScan
-- Fusion

-- TODO better accumulator needed, one that does not ignore the value
counter = \c v -> c+1

mapScanPre  = streamScan counter 0 . streamMap succ
mapScanPost = streamScan (flip (flip counter . succ)) 0

prop_mapScan :: Stream Int -> Bool
prop_mapScan s = mapScanPre s == mapScanPost s

------------------------------------------------------------------------------
-- streamMap → streamWindow
-- window . map f = map (map f) window
-- 7

mapWindowPre :: Stream Char -> Stream [Char]
mapWindowPre     = streamWindow (chop 2) . streamMap succ
mapWindowPost    = streamMap (map succ) . streamWindow (chop 2)
prop_mapWindow s = mapWindowPre s == mapWindowPost s

------------------------------------------------------------------------------
-- streamMap → streamExpand

-- Nothing

------------------------------------------------------------------------------
-- streamMap → streamMerge

--   streamMerge (streamMap f x1) (streamMap f x2)
-- = streamMap f . streamMerge x1 x2

mapMergePre  s = streamMerge [(streamMap next sA),(streamMap next s)]
mapMergePost s = streamMap next $ streamMerge [sA,s]

-- no longer completes after making sA infinite
pxxp_mapMerge s = mapMergePre s == mapMergePost s

------------------------------------------------------------------------------
-- streamMap → streamJoin
-- 8

mapJoinPre     = streamJoin sA . streamMap succ
mapJoinPost    = streamMap (\(x,y) -> (x, succ y)) . streamJoin sA
prop_mapJoin  :: Stream Char -> Bool
prop_mapJoin s = mapJoinPre s == mapJoinPost s

------------------------------------------------------------------------------
-- streamScan → streamFilter
-- streamScan → streamFilterAcc
------------------------------------------------------------------------------
-- streamScan → streamMap

-- Not possible? the problem is the succ-processed accumulator output is fed
-- back in
scanMapPre  = streamMap succ . streamScan counter 0
scanMapPost :: Stream Char -> Stream Int
scanMapPost = streamScan (\a v -> succ (counter a v)) 0

--prop_scanMap s = scanMapPre s == scanMapPost s

------------------------------------------------------------------------------
-- streamScan → streamScan

-- this doesn't work yet. the signature of the impl of mkScanFuse below is
-- Stream (a,a1) -> Stream (t->t, t1 -> a1); consuming and producing a tuple.
-- the problem is the accumulator is not hidden, it's the return value of the
-- function, so we can't easily hide our work!

--mkScanFuse :: (b -> a -> b) -> b
           -- -> (c -> b -> c) -> c
           -- -> Stream a -> Stream b
------mkScanFuse f1 acc1 f2 acc2 = streamScan
    --f2.(f1 acc1) acc2

--    (\v (x,y) -> (f1 x, f2 y))
--    (acc1, acc2)

-- (b -> a -> b) -> b           -> Sa -> Sb
-- (c -> b -> c) -> c           -> Sb -> Sc

foo c v = if c `mod` 10==0 then 1 else v
bar c v = c + 1

scanScanPre = streamScan foo 1 . streamScan bar 0
--scanScanPost = mkScanFuse 

------------------------------------------------------------------------------
-- streamScan → streamWindow
-- streamScan → streamExpand
-- streamScan → streamMerge
-- streamScan → streamJoin

------------------------------------------------------------------------------
-- streamWindow → streamFilter

-- Nothing

------------------------------------------------------------------------------
-- streamWindow → streamFilterAcc

------------------------------------------------------------------------------
-- streamWindow → streamMap

-- Nothing

------------------------------------------------------------------------------
-- streamWindow → streamScan

------------------------------------------------------------------------------
-- streamWindow → streamWindow

-- Nothing

-------------------------------------------------------------------------------
-- streamWindow → streamExpand
-- 9

windowExpandPre n    = streamExpand . streamWindow (chop n)
prop_windowExpand1  :: Stream Char -> Bool
prop_windowExpand1 s = (windowExpandPre 2) s == s

-- works but expensive to evaluate
pxxp_windowExpand2 :: Int -> Stream Char -> Bool
pxxp_windowExpand2 n s = (windowExpandPre n) s == s

-------------------------------------------------------------------------------
-- streamWindow → streamMerge
-- (window, window) → merge
-- merge (window wm1 xs) (window wm2 ys)
-- window wm3 . merge (xs ys)
--
-- concrete example
--
-- merge (window (chop 5) ['0'..]) (window (chop 5) ['a'..])
-- = merge ["01234", "56789"...] ["abcde", "fghij"...]
-- = ["01234", "abcde", "56789", "fghij"...]
-- (or does it? are these the semantics of merge?)
-- 
-- reworking:
-- window frob . merge ['0'..] ['a'..]

frob :: WindowMaker a -- Stream a -> [Stream a]
frob [] = []
frob s = let [x1,y1,x2,y2,x3,y3,x4,y4,x5,y5] = take 10 s
         in [x1,x2,x3,x4,x5] : [y1,y2,y3,y4,y5] : (frob (drop 10 s))

windowMergePre s = streamMerge [ streamWindow (chop 5) sA
                               , streamWindow (chop 5) s ]

windowMergePost s = streamWindow frob (streamMerge [sA,s])

test_windowMerge = assertBool $ take 10 (windowMergePre sB) == take 10 (windowMergePost sB)

-- XXX doesn't work: failing on an empty list?
-- Behaviour when sampling <10 is different (due to frob impl)
prop_windowMerge s = windowMergePre s == windowMergePost s

------------------------------------------------------------------------------
-- streamWindow → streamJoin

------------------------------------------------------------------------------
-- streamExpand → streamFilter
-- 10

expandFilterPre     = streamFilter f . streamExpand
expandFilterPost    = streamExpand . streamMap (filter f)
prop_expandFilter s = expandFilterPre s == expandFilterPost s

------------------------------------------------------------------------------
-- streamExpand → streamFilterAcc

------------------------------------------------------------------------------
-- streamExpand → streamMap
-- 11

expandMapPre     = streamMap succ . streamExpand
expandMapPost    = streamExpand . streamMap (map succ)
prop_expandMap :: Stream [Char] -> Bool
prop_expandMap s = expandMapPre s == expandMapPost s

------------------------------------------------------------------------------
-- streamExpand → streamScan

------------------------------------------------------------------------------
-- streamExpand → streamWindow
-- 12

-- in the case where the window size coming in matches that being constructed,
-- this is elimination

expandWindowPre1 n= streamWindow (chop n) . streamExpand
expandWindowPost1 = id

-- XXX it would be nice to use quickCheck to choose a window size, but we need
-- to limit it to very small numbers (<10 or so) and that's tricky to specify;
-- and HTF does not support QuickCheck's guard scheme n < 10 ==> ...
prop_expandWindow1 :: Stream Char -> Bool
prop_expandWindow1 s = expandWindowPre1 2 w == expandWindowPost1 w
    where w = streamWindow (chop 2) s

-- what about other cases?
-- (explore perhaps using one of the accumulators?)

------------------------------------------------------------------------------
-- streamExpand → streamExpand

-- Nothing.

------------------------------------------------------------------------------
-- streamExpand → streamMerge

expandMergePre s = streamMerge [ streamExpand sW, streamExpand s ]

expandMergePost s = streamExpand (streamMerge [ sW, s ])

-- ordering differs:
-- "internal" ordering difference?
-- map expandMerge [2..20] => 011101110111...
expandMerge n = sort (take n (expandMergePre sW))
             == sort (take n (expandMergePost sW))
-- passes
test_expandMerge = assertBool $ expandMerge 20
-- this will run for a long time
-- passes
pxxp_expandMerge s = sort(expandMergePre s) == sort(expandMergePost s)
{-
    expandMergePre => "0011223344"...
    expandMergePost=> "0101232345"...
 -}

------------------------------------------------------------------------------
-- streamExpand → streamJoin

------------------------------------------------------------------------------
-- streamMerge → streamFilter

mergeFilterPre  s = streamFilter f $ streamMerge [sA, s]
mergeFilterPost s = streamMerge [streamFilter f sA, streamFilter f s]

-- XXX ordering not preserved?

-- *very* expensive to evaluate
-- passes
pxxp_mergeFilter s = sort (mergeFilterPre s) == sort (mergeFilterPost s)

------------------------------------------------------------------------------
-- streamMerge → streamFilterAcc
------------------------------------------------------------------------------
-- streamMerge → streamMap

-- using isAscii as our function
mergeMapPre s  = streamMap isAscii $ streamMerge [sA, s]
mergeMapPost s = streamMerge [streamMap isAscii sA, streamMap isAscii s]

-- expensive to evaluate
-- passes
pxxp_mergeMap s = mergeMapPre s == mergeMapPost s

------------------------------------------------------------------------------
-- streamMerge → streamScan
-- streamMerge → streamWindow
------------------------------------------------------------------------------
-- streamMerge → streamExpand

mergeExpandPre s = streamExpand (streamMerge [w1,w2]) where
    w1 = streamWindow (chop 2) sA
    w2 = streamWindow (chop 2) s

mergeExpandPost s = streamMerge [streamExpand w1, streamExpand w2] where
    w1 = streamWindow (chop 2) sA
    w2 = streamWindow (chop 2) s

-- *very* expensive to evaluate
-- passes
pxxp_mergeExpand s = sort (mergeExpandPre s) == sort (mergeExpandPost s)

------------------------------------------------------------------------------
-- streamMerge → streamMerge

mergeMergePre c  = streamMerge [sA, streamMerge [sB,c]]
mergeMergePost c = streamMerge [sA, sB, c]

-- ordering:
--  streamMerge [sA,sB,sC] = a0bAc1dBe2fCg3hDi4jE…
--                         = 12131213121312131213
--                           (odd pattern!)
-- streamMerge [sA, streamMerge [sB, sC]]
--                         = a0bAc1dBe2fCg3hDi4jE…
--                           12131213121312131213
--                           same!
--
-- so streammerge is right-associative?

-- streamMerge [streamMerge [sA, sB], sC]
--                         = aA0BbC1DcE2FdG3HeI4J
--                           13231323132313231323
-- (different)

-- passes but very expensive
pxxp_mergeMerge s = mergeMergePre s == mergeMergePost s

------------------------------------------------------------------------------
-- streamMerge → streamJoin
------------------------------------------------------------------------------
-- streamJoin → streamFilter, streamFilterAcc, streamMap, streamScan,
--              streamWindow, streamExpand, streamMerge, streamJoin
--
-- Nothing possible.
