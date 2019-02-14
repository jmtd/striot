= Rewrites
Jonathan Dowland <jon.dowland@ncl.ac.uk>
:toc: right
:toclevels: 4
:code: 

//////////////////////////////////////////////////////////////////////////////
// boilerplate Haskell code that has to be at the start.
// Utility code is at the end.
\begin{code}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Rewrites (htf_thisModulesTests) where

import Data.List (sort)
import Data.Char (isAscii)
import Test.Framework
import Striot.FunctionalIoTtypes
import Striot.FunctionalProcessing

\end{code}
//////////////////////////////////////////////////////////////////////////////

== Abstract

We describe STRIoT: a stream-processing system built around pure
functional programming principles. STRIoT supports the composition of a
stream processing pipeline from eight purely functional operators. The
STRIoT system is designed to rewrite the supplied stream pipeline in
order to optimize its operation against a set of functional and
non-functional requirements. We compile a list of rewrite rules based on
a systematic pairwise comparison of the functional operators. We
classify each rule as total if it can be applied in any circumstance, or
partial if there are requirements on the input stream not expressed by
the type of the stream, and quantify those requirements.

== Background

description of striot
selection of operators

A *Stream* is a list of *Event*s. An *Event* has an Id field and might include an
iota of data, (a datum?) and a timestamp.

<table of operators>

    streamFilter    :: (a -> Bool) -> Stream a -> Stream a
    streamMap       :: (a -> b) -> Stream a -> Stream b
    streamFilterAcc :: (b -> a -> b) -> b -> (a -> b -> Bool) -> Stream a -> Stream a
    streamScan      :: (b -> a -> b) -> b -> Stream a -> Stream b
    streamWindow    :: Stream a -> Stream [a]
    streamExpand    :: Stream [a] -> Stream a
    streamMerge     :: [Stream a] -> Stream a
    streamJoin      :: Stream a -> Stream b -> Stream (a,b)

Ignoring the stream-in, stream-out parameters, the first four operators are
unary and their functional parameter operates on the payload of the stream,
and not the stream itself (the types of the HoFs do not include Stream)

The second set of four operators operate directly on Streams.

== Method

We constructed a list of operator pairs and considered each pair in
sequence in order to systematically explore all possible combinations.
For each pair, we initially attempted to reverse the order of the
operators whilst preserving the functional result of their combination
on an input stream. For pairs of the same
operator, we attempt to construct an equivalent, single, fused operator.
For pairs of operators that are the dual of one another, we attempted to
eliminate them.

This method sometimes yielded a result that turned out not to behave
identically to the original pair in some or all circumstances. If we
thought the rewrite could still be useful in practise, we recorded it
with the appropriate annotation.

Once we had determined a viable rewrite rule, we examined whether its
inverse was a second, distinct viable rule.

=== Semantic analysis

`streamMap` and `streamFilter` are stream variants of the widely used
`map` and `filter` which have well understood semantics.

`streamWindow`, `streamExpand` and `streamJoin` manipulate the structure of the
stream itself: `streamWindow` eliminates the Event wrappers around the iotas
that flow into it and synthesises new ones for the collected iotes it outputs.
`streamExpand` is its dual. `streamJoin` eliminates two `Event` wrapper and
synthesises a new one containing a tuple.

For this reason, the following rewrite rule is not applicable
in all cases:

    streamExpand . streamWindow _ = id

This holds only in terms of the data encapsulated within the stream. But
since `streamWindow` discards the `Event` wrappers around the iotas flowing
into it, we lose the information contained therein, including the timestamp
field.

The type of `streamMerge` indicates that it could manipulate the input
*Event*s, but we know in practise that it doesn't.

=== Tools

Most of this exercise was conducted offline using pen and paper. For
some of the more complex rewrites, we sketched an outline on paper and
then attempted to encode the result in Haskell. Two Haskell libraries in
particular proved very useful for testing our work:
QuickCheck<<QuickCheck>> and
the Haskell Testing Framework<<HTF>>. Using these tools, we attempted to
construct a "property" that expressed the equivalence of the operator
pair before and after our rewrite. QuickCheck then generates a set of
randomly generated inputs and compares the output of each side of the
equation.

This provided some confidence that each rewrite rule was correct. It
also helped to catch a few cases where we had authored a rewrite rule we
thought was total, but wasn't. We then explored the failure cases to
determine whether the rule needed to be thrown out, or was useful in
some circumstances, in which case we noted the necessary caveats.

TODO "total" mentioned in the para above; but defined below

In order for QuickCheck to generate random Stream data, we were required
to provide a trivial implementation of the Arbitrary class for the
appropriate data type in STRIoT. This data type is higher-order, and so
to evaluate the properties, we needed to bind the type variables to
something concrete.  We chose `Char`, which has several nice properties:
it satisfies `Eq`, permitting the comparison of streams of `Chars`; it
satisfies `Ord`, simplifying the writing of predicates for
`streamFilter` and friends (we can use standard functions including `>=`
and `succ`).  Finally, when examining the output of operator-pairs in an
interactive session, `Char` is quite friendly to human eyes.

=== Describing rewrites

==== total/partial

We describe a rewrite as total if it can be applied to any occurance of
the pattern. Some rewrites are only applicable if some other properties
of the stream hold. These properties are not expressed in the definition
or type of the rewrite function. Indeed they may not be expressible due
to the limits of the type system.

An example of a partial rewrite rule might be one that does not guarantee that
the ordering of the input Events is preserved in the rewritten version.
Another is the elimination of adjacent window/expand operations, which result
in the loss of the timestamp and id metadata from the input Events.  In both
cases whether these are important considerations is application-specific.

== TODO

Classification of non-order-preserving rewrites: whether the re-ordering
is determined *internally* or *externally*. Internally means that
the re-ordering is entirely determined by the composition of stream
operators; external means that it is dependent on one of the externally
supplied arguments, such as the predicate supplied to streamFilter.

== Results

//////////////////////////////////////////////////////////////////////////////
// To ensure all combinations have been covered, all pairs are listed here in
// order, with those which do not yield a rewrite rule commented out.

F  01: streamFilter . streamFilter
X1 02: streamMap . streamFilter
F  03: streamFilterAcc . streamFilter
X1 04: streamScan . streamFilter
X  05: streamWindow . streamFilter
X4 06: streamExpand . streamFilter
X5 07: streamJoin . streamFilter
   08: streamMerge . streamFilter
2  09: streamFilter . streamMap
F  10: streamMap . streamMap
5  11: streamFilterAcc . streamMap
   12: streamScan . streamMap
X  13: streamWindow . streamMap
X4 14: streamExpand . streamMap
   15: streamJoin . streamMap
   16: streamMerge . streamMap
F  17: streamFilter . streamFilterAcc
X1 18: streamMap . streamFilterAcc
F  19: streamFilterAcc . streamFilterAcc
   20: streamScan . streamFilterAcc
X  21: streamWindow . streamFilterAcc
X4 22: streamExpand . streamFilterAcc
X  23: streamJoin . streamFilterAcc
X8 24: streamMerge . streamFilterAcc
   25: streamFilter . streamScan
   26: streamMap . streamScan
   27: streamFilterAcc . streamScan
X6 28: streamScan . streamScan
X  29: streamWindow . streamScan
X4 30: streamExpand . streamScan
   31: streamJoin . streamScan
   32: streamMerge . streamScan
   33: streamFilter . streamWindow
   34: streamMap . streamWindow
   35: streamFilterAcc . streamWindow
   36: streamScan . streamWindow
   37: streamWindow . streamWindow
X3 38: streamExpand . streamWindow
X  39: streamJoin . streamWindow
   40: streamMerge . streamWindow
   41: streamFilter . streamExpand
3  42: streamMap . streamExpand
   43: streamFilterAcc . streamExpand
   44: streamScan . streamExpand
   45: streamWindow . streamExpand
4  46: streamExpand . streamExpand
X  47: streamJoin . streamExpand
   48: streamMerge . streamExpand
X2 49: streamFilter . streamJoin
X2 50: streamMap . streamJoin
X2 51: streamFilterAcc . streamJoin
X2 52: streamScan . streamJoin
X2 53: streamWindow . streamJoin
X2 54: streamExpand . streamJoin
X2 55: streamJoin . streamJoin
X2 56: streamMerge . streamJoin
8  57: streamFilter . streamMerge
7  58: streamMap . streamMerge
X8 59: streamFilterAcc . streamMerge
X8 60: streamScan . streamMerge
   61: streamWindow . streamMerge
   62: streamExpand . streamMerge
X  63: streamJoin . streamMerge
F  64: streamMerge . streamMerge

//////////////////////////////////////////////////////////////////////////////

1. `streamFilter` fusion (total)

\begin{code}
------------------------------------------------------------------------------
filterFilterPre     = streamFilter g . streamFilter f
filterFilterPost    = streamFilter (\x -> f x && g x)
prop_filterFilter s = filterFilterPre s == filterFilterPost s
------------------------------------------------------------------------------
\end{code}

[start=2]
2. `streamFilterAcc` and `streamFilter` fusion (total)

\begin{code}
------------------------------------------------------------------------------
filterAccFilterPre     = streamFilter g . streamFilterAcc accfn1 acc1 pred1
filterAccFilterPost    = streamFilterAcc accfn1 acc1 (\x a -> pred1 x a && g x)
prop_filterAccFilter s = filterAccFilterPre s == filterAccFilterPost s
------------------------------------------------------------------------------
\end{code}

[start=3]
3. `streamFilter` and `streamFilterAcc` fusion (total)

\begin{code}
------------------------------------------------------------------------------
filterFilterAccPre     = streamFilterAcc accfn1 acc1 pred1 . streamFilter g
filterFilterAccPost    =
    streamFilterAcc
        (\a v -> if g v then accfn1 a v else a)
        acc1
        (\x a -> g x && pred1 x a)
prop_filterFilterAcc s = filterFilterAccPre s == filterFilterAccPost s
------------------------------------------------------------------------------
\end{code}

[start=4]
4. `streamFilterAcc` fusion (total)

\begin{code}
------------------------------------------------------------------------------
filterAccFilterAccPre     = streamFilterAcc accfn2 acc2 pred2 . streamFilterAcc accfn1 acc1 pred1
filterAccFilterAccPost    =
    streamFilterAcc
        (\(x,y) v -> (accfn1 x v, if pred1 v x then accfn2 y v else y))
        (acc1,acc2)
        (\x (y,z) -> pred1 x y && pred2 x z)
prop_filterAccFilterAcc s = filterAccFilterAccPre s == filterAccFilterAccPost s
------------------------------------------------------------------------------
\end{code}

[start=5]
5. `streamMap` into `streamFilter`
   Where `next` is the example map function (chooses the next item in a sequence
   and wraps from the end to the start).
   
   If p is highly selective, then the overhead of evaluating f
   twice per selected event may be lower than the savings made by
   reducing the list de/reconstruction overhead of streamMap.

\begin{code}
------------------------------------------------------------------------------
-- TODO choice of f for filter and next for map is not particularly generic
-- perhaps p for filter and f for map
mapFilterPre     = streamFilter f . streamMap next
mapFilterPost    = streamMap next . streamFilter (f . next)
prop_mapFilter s = mapFilterPre s == mapFilterPost s
------------------------------------------------------------------------------
\end{code}

[begin=6]
6. `streamMap` fusion (total)

\begin{code}
------------------------------------------------------------------------------
mapMapPre :: Stream Char -> Stream Char
mapMapPre     = streamMap next . streamMap next
mapMapPost    = streamMap (next . next)
prop_mapMap s = mapMapPre s == mapMapPost s
------------------------------------------------------------------------------
\end{code}

[begin=7]
7. `streamMap` into `streamJoin` (total)

\begin{code}
------------------------------------------------------------------------------
mapJoinPre     = streamJoin sA . streamMap next
mapJoinPost    = streamMap (\(x,y) -> (x, next y)) . streamJoin sA
prop_mapJoin  :: Stream Char -> Bool
prop_mapJoin s = mapJoinPre s == mapJoinPost s
------------------------------------------------------------------------------
\end{code}


[start=8]
8. `streamExpand` into `streamFilter` (total)
   TODO consider the Event wrappers

\begin{code}
------------------------------------------------------------------------------
expandFilterPre     = streamFilter f . streamExpand
expandFilterPost    = streamExpand . streamMap (filter f)
prop_expandFilter s = expandFilterPre s == expandFilterPost s
------------------------------------------------------------------------------
\end{code}

[start=9]
9. `streamExpand` into `streamMap` (total)
   TODO consider the Event wrappers

\begin{code}
------------------------------------------------------------------------------
expandMapPre     = streamMap next . streamExpand
expandMapPost    = streamExpand . streamMap (map next)
prop_expandMap :: Stream [Char] -> Bool
prop_expandMap s = expandMapPre s == expandMapPost s
------------------------------------------------------------------------------
\end{code}

[start=10]
10. `streamMerge` into `streamMap`
     (total)

\begin{code}
------------------------------------------------------------------------------
mergeMapPre s   = streamMap isAscii $ streamMerge [sA, s]
mergeMapPost s  = streamMerge [streamMap isAscii sA, streamMap isAscii s]
-- expensive to evaluate, but passes
pxxp_mergeMap s = mergeMapPre s == mergeMapPost s
------------------------------------------------------------------------------
\end{code}

[start=11]
11. `streamMerge` into `streamMerge`
    (total)
    ordering preserved in the right-associative case

\begin{code}
------------------------------------------------------------------------------
mergeMergePre c   = streamMerge [sA, streamMerge [sB,c]]
mergeMergePost c  = streamMerge [sA, sB, c]
pxxp_mergeMerge s = mergeMergePre s == mergeMergePost s
------------------------------------------------------------------------------
\end{code}

[start=12]
12. `streamMap` into `streamFilterAcc` (total)

\begin{code}
------------------------------------------------------------------------------
mapFilterAccPre     = streamFilterAcc accfn 0 accpred . streamMap next
mapFilterAccPost    = streamMap next . streamFilterAcc accfn 0 (accpred . next)
prop_mapFilterAcc :: Stream Char -> Bool
prop_mapFilterAcc s = mapFilterAccPre s == mapFilterAccPost s
------------------------------------------------------------------------------
\end{code}

[start=13]
13. `streamMap` into `streamScan`: a variant of fusion (total)

\begin{code}
------------------------------------------------------------------------------
mapScanPre     = streamScan scanfn 0 . streamMap next
mapScanPost    = streamScan (flip (flip scanfn . next)) 0

prop_mapScan :: Stream Int -> Bool
prop_mapScan s = mapScanPre s == mapScanPost s
------------------------------------------------------------------------------
\end{code}

=== Inverted rules

When the above rules were derived, each was analysed to determine
whether it could be inverted: whether any occurence of a stream matching
the pattern on the right could be replaced with that on the left.

The following rules are inverted versions of the above, but are subject to a
*decomposition caveat*: The arguments to the stream operators on
the left hand side of these rules are *compound expressions*, that are
decomposed and their constituent expressions used on the right-hand side.

When we are considering a practical system of applying such rules to a
Stream Graph, it is unlikely that we are going to be able to decompose or
inspect the composition of the functional arguments, so these rules may
be of limited practical use.

//////////////////////////////////////////////////////////////////////////////
// there's no value in Haskell implementations for these
//////////////////////////////////////////////////////////////////////////////

[start=14]
14. `filter (\x -> f x && g x) = filter f . filter g`

15. `streamMap f . streamFilter (p . f) = streamFilter p . streamMap f`

16. `streamMap (f . g) = streamMap f . streamMap g`

17. `streamMap (\(x,y) -> (x, f y)) . streamJoin s1 = streamJoin s1 . streamMap f`

18. `streamExpand . streamMap (filter f) = streamFilter f .  streamExpand`

19. `streamExpand . streamMap (map f) = streamMap f . streamExpand`

20. `streamMap f . streamFilterAcc af a (p . f)
    = streamFilterAcc af a p . streamMap f`

==== inverted `streamMerge` rules

The semantics of `streamMerge` are unique amongst the stream operators, given
its unique type signature.

TODO expand

[start=21]
21. `streamMerge [streamMap f s1, streamMap f s2]
        = streamMap f $ streamMerge [s1, s2]`

 TODO this is kind of a special case of the composition caveat?

22. `streamMerge [s1, s2, s3] = streamMerge [s1, streamMerge [s2, s3]]`

=== Partial rules

The following rules do not preserve the metadata contained within the Event
structures. TODO what do they do to "empty" events? I.e. Nothing instead of
a datum? are they discarded in the window function?

[start=23]
23. `streamExpand . streamWindow _ = id`

24. `streamWindow w . streamMap f = streamMap (map f) . streamWindow w`

    TODO:
    only works if streamWindow predicate does not look at value:
    window (>=3) . map (+1) [1,2,3,4] ≠ map (+1) . window (>=3) [1,2,3,4]
    otoh that's not a valid windowmaker either.

The following partial rules do not preserve the order of stream events:

[start=25]
25. `streamMerge [streamExpand s1, streamExpand s2]
    = streamExpand (streamMerge [s2,s2])`

\begin{code}
------------------------------------------------------------------------------
expandMergePre s   = streamMerge [ streamExpand sW, streamExpand s ]
expandMergePost s  = streamExpand (streamMerge [ sW, s ])
-- this is very slow to execute but passes
pxxp_expandMerge s = sort(expandMergePre s) == sort(expandMergePost s)
------------------------------------------------------------------------------
\end{code}

[start=26]
26. `streamMerge [streamFilter f s1, streamFilter f s2]
    = streamFilter f $ streammerge [s1, s2]`

\begin{code}
------------------------------------------------------------------------------
filterMergePre  s  = streamMerge [streamFilter f sA, streamFilter f s]
filterMergePost s  = streamFilter f $ streamMerge [sA, s]
-- this is very slow to execute but passes
pxxp_filterMerge s = sort (filterMergePre s) == sort (filterMergePost s)
------------------------------------------------------------------------------
\end{code}

and their inverses

[start=27]
    27. `streamExpand (streamMerge [s1,s2])
        = streamMerge [streamExpand s2, streamExpand s2]`

    28. `streamFilter f $ streammerge [s1, s2]
        = streamMerge [streamFilter f s1, streamFilter f s2]`

There are some issues to consider about constant or variable size of
lists in the case where the stream data type is a list, such as after
a streamWindow operator. In the case of streamWindow, the output list
size will be constant, but this is not reflected in the type.
(TODO : where does this matter?)

=== Summary

15 rules
23 adding inversions (with caveats)
28 adding partial rules that do not preserve re-ordering

It appears to not be possible to perform the same promotion/fusion trick
with streamScan as streamFilterAcc (TODO: Why?)

=== join

For pairs where the first operator is join, we know that the second
must operate on a tuple. However we cannot use this information to
decompose the arguments to higher order functions (filter or map), so in
general it seems no useful rewrites exist for this category of pairs.

== Conclusion

There are 64 pairings of 8 functional operators. Systematically looking
for ways to rewrite each pair whilst preserving the functional
definition yielded up to 27 rewrite rules: 12 rules classified as
applicable in any circumstance (total), a further 7 with caveats
discovered by testing for inversions of the first 12, and a further
8 partial rules that apply if certain external properties of the stream
apply (such as, strict order not being important)

These rules may prove useful as a base set of possible rewrites that
could be applied to a stream processing graph in order to change and/or
optimise the non-functional behaviour of the graph.

Examination of the rules, in particular the partial rules, has revealed
some properties of the graphs that, if encoded and provided to a rewrite
system, could aid in making more effective rewriting decisions. For
example if strict ordering of stream events is not important, then a
further 6 rewrite rules could be applied.

=== Further work

 * looking at triples or other combinations of operators
 * factoring in consideration of partitions
 * selection and encoding of additional information about streams for
   rewrite purposes

[bibliography]
== References

- [[[QuickCheck]]]
- [[[HTF]]]

//////////////////////////////////////////////////////////////////////////////
// Utility Haskell code, required by the inline examples
\begin{code}

main = htfMain htf_thisModulesTests

-- filter predicates
f = (>= 'a')
g = (<= 'z')

-- example arguments for streamFilterAcc
-- alternating values only
accfn2 _ v = v
acc2 = '\NUL'
pred2 = (/=)

-- increasing values only
accfn1 _ v = v
acc1 = '\NUL'
pred1 = (>=)

-- avoids a situation where pred/succ will fail on the smallest/largest Enum type
next :: (Eq a, Bounded a, Enum a) => a -> a
next a = if a == maxBound then minBound else succ a
prev :: (Eq a, Bounded a, Enum a) => a -> a
prev a = if a == minBound then maxBound else pred a

-- test streams of characters
sA = [Event 0 Nothing (Just i)|i<-iterate next 'a']
sB = [Event 0 Nothing (Just i)|i<-iterate next '0']
sC = [Event 0 Nothing (Just i)|i<-iterate next 'A']
sW = streamWindow (chop 2) sB

-- utility functions for mapFilterAcc
accfn acc _ = acc+1
accpred dat acc = even acc

-- an example of a streamScan argument
-- TODO better accumulator needed, one that does not ignore the value
counter = \c v -> c+1
scanfn  = counter

\end{code}
//////////////////////////////////////////////////////////////////////////////
