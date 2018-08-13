import Striot.FunctionalIoTtypes
import Striot.FunctionalProcessing

{-
    possible StreamGraph rewrites, via combination of operators
 -}

-------------------------------------------------------------------------------
-- useful functions

jClean = map (\(Event _ _ (Just v)) -> v)

-------------------------------------------------------------------------------
-- streamFilter → streamFilter
-- streamFilter → streamFilterAcc
-- streamFilter → streamMap
-- streamFilter → streamScan
-- streamFilter → streamWindow
-- streamFilter → streamExpand
-- streamFilter → streamMerge
-- streamFilter → streamJoin
-- streamFilterAcc → streamFilter
-- streamFilterAcc → streamFilterAcc
-- streamFilterAcc → streamMap
-- streamFilterAcc → streamScan
-- streamFilterAcc → streamWindow
-- streamFilterAcc → streamExpand
-- streamFilterAcc → streamMerge
-- streamFilterAcc → streamJoin
-- streamMap → streamFilter
-- streamMap → streamFilterAcc
-- streamMap → streamMap
-- streamMap → streamScan
-- streamMap → streamWindow
-- streamMap → streamExpand
-- streamMap → streamMerge
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

s4 = [Event 0 Nothing (Just i)|i<-['0'..]]
sA = [Event 0 Nothing (Just i)|i<-['a'..]]

windowMergePre = streamMerge [ streamWindow (chop 5) s4
                             , streamWindow (chop 5) sA ]

-- XXX can we make this point-free?
windowMergePost = streamWindow frob (streamMerge [s4,sA])

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
-- streamMerge → streamFilter
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
