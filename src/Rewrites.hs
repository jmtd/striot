{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Rewrites (htf_thisModulesTests) where

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

sA = [Event 0 Nothing (Just i)|i<-['a'..]]
sB = [Event 0 Nothing (Just i)|i<-['0'..]]
sW = streamWindow (chop 2) sB

instance Arbitrary a => Arbitrary (Event a) where
    arbitrary = do
        c <- arbitrary
        return $ Event 0 Nothing (Just c)

mkTest pre post stream = assertBool $ take 10 (pre stream) == take 10 (post stream)

-------------------------------------------------------------------------------
-- streamFilter → streamFilter
-- streamFilter g . streamFilter f = streamFilter (\x -> g x && f x)

f = (>= 'a')
g = (<= 'z')

filterFilterPre  = streamFilter g . streamFilter f
filterFilterPost = streamFilter (\x -> f x && g x)

test_filterFilter = mkTest filterFilterPre filterFilterPost sB

prop_filterFilter s = filterFilterPre s == filterFilterPost s

-------------------------------------------------------------------------------
-- streamFilter → streamFilterAcc

-------------------------------------------------------------------------------
-- streamFilter → streamMap
-- streamMap f . streamFilter p = streamFilter (p . f) . streamMap f

ff x = (iterate pred x) !! 32 -- 'a' -> 'A'
p x  = x >= 'a' && x <= 'z'

filterMapPre  = (streamMap ff . streamFilter p) sB
filterMapPost = (streamFilter (p . ff) . streamMap ff) sB
-- this is wrong. we need the inverse of ff for the new predicate; and we can't
-- infer that from the types

-------------------------------------------------------------------------------
-- streamFilter → streamScan

-------------------------------------------------------------------------------
-- streamFilter → streamWindow
-- streamFilter → streamExpand

-- Nothing

------------------------------------------------------------------------------
-- streamFilter → streamMerge

filterMergePre  s = streamMerge [streamFilter f sA, streamFilter f s]
filterMergePost s = streamFilter f $ streamMerge [sA, s]

-- false! this only works if ordering is not important, or if we can re-order
-- post merge (sort on a timestamp?)
txxt_filterMerge = assertBool $ take 10 (filterMergePre sB) == take 10 (filterMergePost sB)
-- these are also very slow to execute
pxxp_filterMerge s = filterMergePre s == filterMergePost s

------------------------------------------------------------------------------
-- streamFilter → streamJoin

-- Nothing

-- streamFilterAcc → streamFilter

---------------------------------------------------------------------------------------
-- streamFilterAcc → streamFilterAcc

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

---------------------------------------------------------------------------------------
-- streamFilterAcc → streamMap
-- streamFilterAcc → streamScan
-- streamFilterAcc → streamWindow
-- streamFilterAcc → streamExpand
-- streamFilterAcc → streamMerge
-- streamFilterAcc → streamJoin

------------------------------------------------------------------------------
-- streamMap → streamFilter
-- streamFilter p . streamMap f = streamMap f . streamFilter (p . f)
-- some kind of trade off depending on how selective p is?
mff = succ

mapFilterPre     = streamFilter f . streamMap mff
mapFilterPost    = streamMap mff . streamFilter (f . mff)
prop_mapFilter s = mapFilterPre s == mapFilterPost s

------------------------------------------------------------------------------
-- streamMap → streamFilterAcc

------------------------------------------------------------------------------
-- streamMap → streamMap
-- fuse

mapMapPre :: Stream Char -> Stream Char
mapMapPre     = streamMap succ . streamMap succ
mapMapPost    = streamMap (succ . succ)
prop_mapMap s = mapMapPre s == mapMapPost s

------------------------------------------------------------------------------
-- streamMap → streamScan

------------------------------------------------------------------------------
-- streamMap → streamWindow
-- window . map f = map (map f) window

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

mapMergePre  s = streamMerge [(streamMap succ sA),(streamMap succ s)]
mapMergePost s = streamMap succ $ streamMerge [sA,s]

test_mapMerge = assertBool $ take 50 (mapMergePre sB) == take 50 (mapMergePost sB)
-- this catches a failure!
prop_mapMerge s = mapMapPre s == mapMergePost s

------------------------------------------------------------------------------
-- streamMap → streamJoin

mapJoinPre     = streamJoin sA . streamMap succ
mapJoinPost    = streamMap (\(x,y) -> (x, succ y)) . streamJoin sA
prop_mapJoin  :: Stream Char -> Bool
prop_mapJoin s = mapJoinPre s == mapJoinPost s

------------------------------------------------------------------------------
-- streamScan → streamFilter
-- streamScan → streamFilterAcc
-- streamScan → streamMap
-- streamScan → streamScan
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

windowExpandPre n    = streamExpand . streamWindow (chop n)
prop_windowExpand1  :: Stream Char -> Bool
prop_windowExpand1 s = (windowExpandPre 2) s == s

-- works but expensive to evaluate
-- prop_windowExpand2 :: Int -> Stream Char -> Bool
-- prop_windowExpand2 n s = (windowExpandPre n) s == s

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

-- doesn't work
prop_windowMerge s = windowMergePre s == windowMergePost s

------------------------------------------------------------------------------
-- streamWindow → streamJoin

------------------------------------------------------------------------------
-- streamExpand → streamFilter

expandFilterPre     = streamFilter f . streamExpand
expandFilterPost    = streamExpand . streamMap (filter f)
prop_expandFilter s = expandFilterPre s == expandFilterPost s

------------------------------------------------------------------------------
-- streamExpand → streamFilterAcc

------------------------------------------------------------------------------
-- streamExpand → streamMap

expandMapPre     = streamMap succ . streamExpand
expandMapPost    = streamExpand . streamMap (map succ)
prop_expandMap :: Stream [Char] -> Bool
prop_expandMap s = expandMapPre s == expandMapPost s

------------------------------------------------------------------------------
-- streamExpand → streamScan

------------------------------------------------------------------------------
-- streamExpand → streamWindow

-- in the case where the window size coming in matches that being constructed,
-- this is elimination

expandWindowPre1  = streamWindow (chop 2) . streamExpand
expandWindowPost1 = id
test_expandWindow1= mkTest expandWindowPre1 expandWindowPost1 sW

-- fails! because the incoming data is not consistent window size of 2
--prop_expandWindow1 :: Stream [Char] -> Bool
--prop_expandWindow1 s = expandWindowPre1 s == expandWindowPost1 s

-- what about other cases?
-- (explore perhaps using one of the accumulators?)

------------------------------------------------------------------------------
-- streamExpand → streamExpand

-- Nothing.

------------------------------------------------------------------------------
-- streamExpand → streamMerge

expandMergePre = streamMerge [ streamExpand sW, streamExpand sW ]

expandMergePost = streamExpand (streamMerge [ sW, sW ])

-- fails: ordering differs
test_expandMerge = assertBool $ take 10 expandMergePre
                             == take 10 expandMergePost
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

-- false! (ordering will have changed)
-- XXX this seems very expensive to evaluate
prop_mergeFilter s = mergeFilterPre s == mergeFilterPost s

-- streamMerge → streamFilterAcc
-- streamMerge → streamMap
-- streamMerge → streamScan
-- streamMerge → streamWindow
-- streamMerge → streamExpand
-- streamMerge → streamMerge
-- streamMerge → streamJoin
-- streamJoin → streamFilter
-- streamJoin → streamFilterAcc
-- streamJoin → streamMap
-- streamJoin → streamScan
-- streamJoin → streamWindow
-- streamJoin → streamExpand
-- streamJoin → streamMerge
-- streamJoin → streamJoin
