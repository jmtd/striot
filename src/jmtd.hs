import Striot.FunctionalIoTtypes
import Striot.FunctionalProcessing

{-
    possible StreamGraph rewrites, via combination of operators
 -}

-------------------------------------------------------------------------------
-- utility functions

jClean = map (\(Event _ _ (Just v)) -> v)

sA = [Event 0 Nothing (Just i)|i<-['a'..]]
sB = [Event 0 Nothing (Just i)|i<-['0'..]]

-------------------------------------------------------------------------------
-- streamFilter → streamFilter
-- streamFilter g . streamFilter f = streamFilter (\x -> g x && f x)

f = (>= 'a')
g = (<= 'z')

filterFilterPre  = (streamFilter g . streamFilter f) sB
filterFilterPost = streamFilter (\x -> f x && g x) sB

test_filterFilter = take 50 filterFilterPre == take 50 filterFilterPost

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
-- streamFilter → streamWindow
-- streamFilter → streamExpand

------------------------------------------------------------------------------
-- streamFilter → streamMerge

filterMergePre  = streamMerge [streamFilter (>'a') sA, streamFilter (>'a') sB]
filterMergePost = streamFilter (>'a') $ streamMerge [sA, sB]

-- false!
test_filterMerge = take 10 filterMergePre == take 10 filterMergePost

-- streamFilter → streamJoin
-- streamFilterAcc → streamFilter
-- streamFilterAcc → streamFilterAcc
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
mfp = (>'a')

mapFilterPre  = (streamFilter mfp . streamMap mff) sB
mapFilterPost = (streamMap mff . streamFilter (mfp . mff)) sB

test_mapFilter = take 20 mapFilterPre == take 20 mapFilterPost

------------------------------------------------------------------------------
-- streamMap → streamFilterAcc

------------------------------------------------------------------------------
-- streamMap → streamMap
-- fuse

mapMapPre  = streamMap succ . streamMap succ
mapMapPost = streamMap (succ . succ)

test_mapMap = take 20 (mapMapPre sA) == take 20 (mapMapPost sA)


-- streamMap → streamScan
-- streamMap → streamWindow
-- streamMap → streamExpand
------------------------------------------------------------------------------
-- streamMap → streamMerge

--   streamMerge (streamMap f x1) (streamMap f x2)
-- = streamMap f . streamMerge x1 x2

mapMergePre  = streamMerge [(streamMap succ sA),(streamMap succ sB)]
mapMergePost = streamMap succ $ streamMerge [sA,sB]

test_mapMerge = take 50 mapMergePre == take 50 mapMergePost

-- streamMap → streamJoin
-- streamScan → streamFilter
-- streamScan → streamFilterAcc
-- streamScan → streamMap
-- streamScan → streamScan
-- streamScan → streamWindow
-- streamScan → streamExpand
-- streamScan → streamMerge
-- streamScan → streamJoin
-- streamWindow → streamFilter
-- streamWindow → streamFilterAcc
-- streamWindow → streamMap
-- streamWindow → streamScan
-- streamWindow → streamWindow
-- streamWindow → streamExpand

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

windowMergePre = streamMerge [ streamWindow (chop 5) sA
                             , streamWindow (chop 5) sB ]

-- XXX can we make this point-free?
windowMergePost = streamWindow frob (streamMerge [sA,sB])

test_windowMerge = take 10 windowMergePre == take 10 windowMergePost

------------------------------------------------------------------------------
-- streamWindow → streamJoin
-- streamExpand → streamFilter
-- streamExpand → streamFilterAcc
-- streamExpand → streamMap
-- streamExpand → streamScan
-- streamExpand → streamWindow
-- streamExpand → streamExpand
-- streamExpand → streamMerge
-- streamExpand → streamJoin

------------------------------------------------------------------------------
-- streamMerge → streamFilter

mergeFilterPre  = streamFilter (>'a') $ streamMerge [sA, sB]
mergeFilterPost = streamMerge [streamFilter (>'a') sA, streamFilter (>'a') sB]

-- false!
test_mergeFilter = take 10 mergeFilterPre == take 10 mergeFilterPost

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
