% A PDF of this document is occasionally uploaded to:
%         https://jmtd.net/tmp/writeup.pdf

% TODO - there's only 31 grid cell comments, there should be 64?

\documentclass[a4paper]{article}
\usepackage[utf8]{inputenc}
\usepackage{listings}
\usepackage{enumitem}
\usepackage{fullpage}
\usepackage{verbatim} % comment environment

% jmtd - convenience function to make code blocks more concise
\ifdefined\MINTEDON
\newenvironment{code}{\VerbatimEnvironment\begin{minted}{haskell}}{\end{minted}}
\else
\lstnewenvironment{code}{\lstset{language=Haskell}}{}
\fi

\begin{document}

\title{Rewrites}
\author{Jonathan Dowland}
%\email{jon.dowland@ncl.ac.uk}
\maketitle

\tableofcontents

\begin{comment}

Boilerplate and utility code.

\begin{code}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Rewrites (htf_thisModulesTests) where

import Data.List (sort)
import Data.Char (isAscii)
import Test.Framework
import Striot.FunctionalIoTtypes
import Striot.FunctionalProcessing
import Data.Maybe
import Control.Arrow ((>>>))

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
-- XXX these are not infinite lists. Does that matter?
sA = [Event 0 Nothing (Just i)|i<-['a'..]]
sB = [Event 0 Nothing (Just i)|i<-['0'..]]
sC = [Event 0 Nothing (Just i)|i<-['A'..]]
sW = streamWindow (chop 2) sB
sWW= streamWindow (chop 3) sW

-- utility functions for mapFilterAcc
accfn acc _ = acc+1
accpred dat acc = even acc

-- an example of a streamScan argument
-- TODO better accumulator needed, one that does not ignore the value
counter = \c v -> c+1
scanfn  = counter

-- for convenient interactive debugging 
jClean = map (\(Event _ _ (Just x)) -> x)
\end{code}
\end{comment}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Abstract}


STRIoT (a stream-processing system) supports the composition of a
stream-processing definition from eight purely functional operators. The STRIoT
system is designed to rewrite the supplied stream-processing definition (a
program) in order to optimize its operation with respect to a set of
non-functional requirements.

We performed a systematic pairwise comparison of the functional operators in
order to derive a list of rewrite rules that could be used in the STRIoT
system.

This paper collects the rules from this process as well as lessons learned
from analysing the semantics of the operators.

\section{Background}

\begin{itemize}
 \item description of striot
 \item selection of operators
\end{itemize}

%%  XXX move elsewhere
A \textit{Stream} is a list of \textit{Event} s. An \textit{Event} has an Id field and might include an
iota of data, (a datum?) and a timestamp.

\subsection{table of operators}

    streamFilter    :: (a -> Bool) -> Stream a -> Stream a
    streamMap       :: (a -> b) -> Stream a -> Stream b
    streamFilterAcc :: (b -> a -> b) -> b -> (a -> b -> Bool) -> Stream a -> Stream a
    streamScan      :: (b -> a -> b) -> b -> Stream a -> Stream b
    streamWindow    :: Stream a -> Stream [a]
    streamExpand    :: Stream [a] -> Stream a
    streamMerge     :: [Stream a] -> Stream a
    streamJoin      :: Stream a -> Stream b -> Stream (a,b)

Ignoring the Stream-in, Stream-out parameters, the first four operators are
unary and their parameter operates on the payload of the Stream,
and not the Stream type itself.

The second set of four operators operate directly on Streams.

\section{Method}

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
with the appropriate annotation. Such rules we consider "partial", in
contrast to the regular, functional-behaviour-preserving rules which
are "total".

Once we had determined a viable rewrite rule, we examined whether its
inverse was a second, distinct viable rule.

\subsection{Semantic analysis}

\texttt{streamMap} and \texttt{streamFilter} are stream variants of the widely used
\texttt{map} and \texttt{filter} which have well understood semantics.

\texttt{streamWindow}, \texttt{streamExpand} and \texttt{streamJoin} manipulate
the structure of the stream itself: \texttt{streamWindow} eliminates the Event
wrappers around the iotas that flow into it and synthesises new ones for the
collected iotes it outputs.  \texttt{streamExpand} is its dual.
\texttt{streamJoin} eliminates two \texttt{Event} wrapper and synthesises a new
one containing a tuple.

For this reason, the following rewrite rule is not applicable
in all cases:

$streamExpand . streamWindow _ = id$

This holds only in terms of the data encapsulated within the stream. But
since \texttt{streamWindow} discards the \texttt{Event} wrappers around the iotas flowing
into it, we lose the information contained therein, including the timestamp
field.

The type of \texttt{streamMerge} indicates that it could manipulate the input
\textit{Event}s, but we know in practice that it doesn't.

\subsection{Tools}

Most of this exercise was conducted offline using pen and paper. For
some of the more complex rewrites, we sketched an outline on paper and
then attempted to encode the result in Haskell. Two Haskell libraries in
particular proved very useful for testing our work:
QuickCheck\cite{Claessen:2000:QLT:351240.351266} and
the Haskell Testing Framework\cite{HTF}. Using these tools, we attempted to
construct a "property" that expressed the equivalence of the operator
pair before and after our rewrite. QuickCheck then generates a set of
randomly generated inputs and compares the output of each side of the
equation.

This provided some confidence that each rewrite rule was correct. It
also helped to catch a few cases where we had authored a rewrite rule we
thought was total, but wasn't. We then explored the failure cases to
determine whether the rule needed to be thrown out, or was useful in
some circumstances, in which case we noted the necessary caveats.

In order for QuickCheck to generate random Stream data, we were required
to provide a trivial implementation of the Arbitrary class for the
appropriate data type in STRIoT. This data type is higher-order, and so
to evaluate the properties, we needed to bind the type variables to
something concrete.  We chose \texttt{Char}, which has several nice properties:
it satisfies \texttt{Eq}, permitting the comparison of streams of \texttt{Chars}; it
satisfies \texttt{Ord}, simplifying the writing of predicates for
\texttt{streamFilter} and friends (we can use standard functions including
\texttt{>=}
and \texttt{succ}).  Finally, when examining the output of operator-pairs in an
interactive session, \texttt{Char} is quite friendly to human eyes.

\subsection{Describing rewrites}

\subsubsection{total/partial}

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

\section{TODO}

Classification of non-order-preserving rewrites: whether the re-ordering
is determined \textit{internally} or \textit{externally}. Internally means that
the re-ordering is entirely determined by the composition of stream
operators; external means that it is dependent on one of the externally
supplied arguments, such as the predicate supplied to streamFilter.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Results}

\subsection{Rewrite rules}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsubsection{Self-evidently useful rules}

Some of the rules are self-evidently useful, such as those which implement
operator fusion: by reducing the number of operators, they reduce the
complexity of the program and the amount of list construction/deconstruction
taking place behind the scenes. The rewritten stream-processing program will
consume less CPU time and/or memory.

All four combinations of \texttt{streamFilter} and \texttt{streamFilterAcc}
can be fused together.

\begin{enumerate}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GRID CELL 1

\item \texttt{streamFilter >>> streamFilter} (fusion)

\begin{code}
filterFilterPre     = streamFilter g . streamFilter f
filterFilterPost    = streamFilter (\x -> f x && g x)
prop_filterFilter s = filterFilterPre s == filterFilterPost s
prop_filterFilter2 s=
    (streamFilter g . streamFilter f) s == streamFilter (\x -> f x && g x) s
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  F  03: streamFilterAcc . streamFilter
%% GRID CELL 3
\item \texttt{streamFilter >>> streamFilterAcc} (fusion)

\begin{code}
filterFilterAccPre     = streamFilterAcc accfn1 acc1 pred1 . streamFilter g
filterFilterAccPost    =
    streamFilterAcc
        (\a v -> if g v then accfn1 a v else a)
        acc1
        (\x a -> g x && pred1 x a)
prop_filterFilterAcc s = filterFilterAccPre s == filterFilterAccPost s
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  F  17: streamFilter . streamFilterAcc

%% GRID CELL 17
\item \texttt{streamFilterAcc >>> streamFilter} (fusion)

\begin{code}
filterAccFilterPre     = streamFilter g . streamFilterAcc accfn1 acc1 pred1
filterAccFilterPost    = streamFilterAcc accfn1 acc1 (\x a -> pred1 x a && g x)
prop_filterAccFilter s = filterAccFilterPre s == filterAccFilterPost s
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  F  19: streamFilterAcc . streamFilterAcc

%% GRID CELL 19
\item \texttt{streamFilterAcc >>> streamFilterAcc} (fusion)

\begin{code}
filterAccFilterAccPre     = streamFilterAcc accfn2 acc2 pred2 . streamFilterAcc accfn1 acc1 pred1
filterAccFilterAccPost    =
    streamFilterAcc
        (\(x,y) v -> (accfn1 x v, if pred1 v x then accfn2 y v else y))
        (acc1,acc2)
        (\x (y,z) -> pred1 x y && pred2 x z)
prop_filterAccFilterAcc s = filterAccFilterAccPre s == filterAccFilterAccPost s
\end{code}

\end{enumerate}

The same is not true of combinations of \texttt{streamMap} and \texttt{streamScan}.

TODO move in explanation of why scan isn't flexible here

\begin{enumerate}[resume]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  F  10: streamMap . streamMap
%% GRID CELL 10
\item \texttt{streamMap >>> streamMap} (fusion)

\begin{code}
mapMapPre :: Stream Char -> Stream Char
mapMapPre     = streamMap next . streamMap next
mapMapPost    = streamMap (next . next)
prop_mapMap s = mapMapPre s == mapMapPost s
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%     12: streamScan . streamMap

%% GRID CELL 12
\item \texttt{streamMap >>> streamScan} (fusion)

\begin{code}
mapScanPre     = streamScan scanfn 0 . streamMap next
mapScanPost    = streamScan (flip (flip scanfn . next)) 0

prop_mapScan :: Stream Int -> Bool
prop_mapScan s = mapScanPre s == mapScanPost s
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  9  41: streamFilter . streamExpand

%% GRID CELL 41
\item \texttt{streamExpand >>> streamFilter}

\begin{code}
expandFilterPre     = streamFilter f . streamExpand
expandFilterPost    = streamExpand . streamMap (filter f)
prop_expandFilter s = expandFilterPre s == expandFilterPost s
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  ?  45: streamWindow . streamExpand
%% GRID CELL 45
\item \texttt{streamWindow w >>> streamExpand >>> streamWindow w = id}

NOTE This is not a more generic \texttt{streamExpand >>> streamWindow}

This is very specific to the WindowMaker in the streamWindow that is not in
consideration here (whatever created the windows that are consumed by
streamExpand.). So it's sooo specific I think this is ruled out.

\begin{code}
expandWindowPre1 n= streamWindow (chop n) . streamExpand
expandWindowPost1 = id

-- XXX it would be nice to use quickCheck to choose a window size, but we need
-- to limit it to very small numbers (<10 or so) and that's tricky to specify;
-- and HTF does not support QuickCheck's guard scheme n < 10 ==> ...
prop_expandWindow1 :: Stream Char -> Bool
prop_expandWindow1 s = expandWindowPre1 2 w == expandWindowPost1 w
    where w = streamWindow (chop 2) s
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  .  64: streamMerge . streamMerge

%% GRID CELL 64
\item \texttt{streamMerge} fusion

\begin{code}
mergeMergePre c  = streamMerge [sA, streamMerge [sB,c]]
mergeMergePost c = streamMerge [sA, sB, c]
-- passes but very expensive
pxxp_mergeMerge s = mergeMergePre s == mergeMergePost s
\end{code}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\end{enumerate}

\subsubsection{not self-evidently useful rules}
% TODO Rename!

\begin{enumerate}[resume]

%%     08: streamMerge . streamFilter
%% TODO

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  2  09: streamFilter . streamMap

%% GRID CELL 9
\item \texttt{streamMap >>> streamFilter}

Where \texttt{next} is the example map function (chooses the next item in a sequence
and wraps from the end to the start).

If p is highly selective, then the overhead of evaluating f
twice per selected event may be lower than the savings made by
reducing the list de/reconstruction overhead of streamMap.

\begin{code}
-- TODO choice of f for filter and next for map is not particularly generic
-- perhaps p for filter and f for map
mapFilterPre     = streamFilter f . streamMap next
mapFilterPost    = streamMap next . streamFilter (f . next)
prop_mapFilter s = mapFilterPre s == mapFilterPost s
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  5  11: streamFilterAcc . streamMap

%% GRID CELL 11
\item \texttt{streamMap >>> streamFilterAcc}

\begin{code}
mapFilterAccPre     = streamFilterAcc accfn 0 accpred . streamMap next
mapFilterAccPost    = streamMap next . streamFilterAcc accfn 0 (accpred . next)
prop_mapFilterAcc :: Stream Char -> Bool
prop_mapFilterAcc s = mapFilterAccPre s == mapFilterAccPost s
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  X  13: streamWindow . streamMap

%% GRID CELL 13
\item \texttt{streamMap >>> streamWindow}

\begin{code}
mapWindowPre :: Stream Char -> Stream [Char]
mapWindowPre     = streamWindow (chop 2) . streamMap next
mapWindowPost    = streamMap (map next) . streamWindow (chop 2)
prop_mapWindow s = mapWindowPre s == mapWindowPost s
\end{code}

%%     15: streamJoin . streamMap

%% GRID CELL 15
\item \texttt{streamMap >>> streamJoin}

\begin{code}
mapJoinPre     = streamJoin sA . streamMap next
mapJoinPost    = streamMap (\(x,y) -> (x, next y)) . streamJoin sA
prop_mapJoin  :: Stream Char -> Bool
prop_mapJoin s = mapJoinPre s == mapJoinPost s
\end{code}

%%  8  16  streamMerge . streamMap

%% GRID CELL 16
\item \texttt{streamMap >>> streamMerge}

\begin{code}
mapMergePre  s = streamMerge [(streamMap next sA),(streamMap next s)]
mapMergePost s = streamMap next $ streamMerge [sA,s]
-- passes but slow
pxxp_mapMerge s = mapMergePre s == mapMergePost s
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  31 31: streamJoin . streamScan

TODO adapting from join . scan

%% GRID CELL 31
\item \texttt{streamScan >>> streamJoin} (total?)

\begin{code}
scanJoinPre     = streamJoin sA . streamScan counter 0
-- one half of the accumulator is never examined, so 'undefined' is used as a
-- placeholder.
scanJoinPost    = streamScan (\c (x,y) -> (x, counter (snd c) y)) (undefined,0) . streamJoin sA
prop_scanJoin  :: Stream Char -> Bool
prop_scanJoin s = scanJoinPre s == scanJoinPost s
\end{code}


%%  ?  32: streamMerge . streamScan
%%  ?  33: streamFilter . streamWindow
%%  ?  34: streamMap . streamWindow
%%  ?  35: streamFilterAcc . streamWindow
%%  ?  36: streamScan . streamWindow
%%  ?  37: streamWindow . streamWindow
%% TODO

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  3  42: streamMap . streamExpand

%% GRID CELL 42
\item \texttt{streamExpand >>> streamMap}

   TODO consider the Event wrappers

\begin{code}
expandMapPre     = streamMap next . streamExpand
expandMapPost    = streamExpand . streamMap (map next)
prop_expandMap :: Stream [Char] -> Bool
prop_expandMap s = expandMapPre s == expandMapPost s
\end{code}

%%  ?  43: streamFilterAcc . streamExpand
%%  ?  44: streamScan . streamExpand

%% GRID CELL 44
\item \texttt{streamExpand >>> streamScan}

TODO possibly not generalised (works for the streamScan 'counter' example)

TODO feed that terminology upwards

\begin{code}

initCounter = 0 :: Int

scanExpandPre  = (streamScan scanfn initCounter). streamExpand

-- we need to propagate the accumulator value between invocations of scanl

-- unlike streamScan, scanl returns the accumulator seed as the first value,
-- without applying the accumulator to it. That's why we have pre-applied the
-- accumulator before passing it scanl. This works for the 'counter' example;
-- I'm not sure yet if it is more general.

-- this change here seems to have forced the input type to Stream [Char] huh?
-- I can only assume it's to do with the type I forced on prop_scanExpand
-- it still passes!
scanExpandPost = streamExpand
    . streamScan (\b a -> tail $ scanl scanfn (last b) a) [initCounter]
    . streamFilter (/=[])

prop_scanExpand :: Stream [Char] -> Bool
prop_scanExpand s = scanExpandPre s == scanExpandPost s

scanfn2 b a = if a > b then 1 else if b == a then 0 else -1
scanAcc2 = 0

scanExpandPre2 = (streamScan scanfn2 scanAcc2) . streamExpand

scanExpandPost2= streamExpand
    . streamScan (\b a -> tail $ scanl scanfn2 (last b) a) [scanAcc2]
    . streamFilter (/=[])

-- TODO the type is forced to Stream [Integer], possibly by scanAcc2,
-- but it shouldn' tbe
prop_scanExpand2 :: Stream [Integer] -> Bool
prop_scanExpand2 s = scanExpandPre2 s == scanExpandPost2 s
\end{code}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  4  46: streamExpand . streamExpand

%% GRID CELL 46
\item \texttt{streamExpand >>> streamExpand}

\begin{code}
expandExpandPre     = streamExpand . streamExpand
expandExpandPost    = streamExpand . streamMap concat
prop_expandExpand :: Stream [[Char]] -> Bool
prop_expandExpand s = expandExpandPre s == expandExpandPost s
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%   7 58: streamMap . streamMerge

%% GRID CELL 58
\item \texttt{streamMerge >>> streamMap}

\begin{code}
mergeMapPre s  = streamMap isAscii $ streamMerge [sA, s]
mergeMapPost s = streamMerge [streamMap isAscii sA, streamMap isAscii s]
-- expensive to evaluate -- passes
pxxp_mergeMap s = mergeMapPre s == mergeMapPost s
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\end{enumerate}
\newcounter{TotalRuleCount}
\setcounter{TotalRuleCount}{\theenumi}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Combinations that yield no rules}

TODO explanations for why the combinations are ruled out where possible

%%  format of rules in comments
%%   X  Y: operator2 . operator1
%%  where X is the reference into the grid for the rule (or no rule, e.g. X1)
%%  and Y is the sequential number for this combination of operators from systematic generation

%%  X1 02: streamMap . streamFilter

02. \texttt{streamFilter >>> streamMap}

In order to apply \texttt{streamFilter` after `streamMap} (with argument type
$(a -> b)$), we need a means of converting the Events into the original type, i.e.,
\textit{(b -> a)}, but we don't have it (in general \texttt{streamMap} is not reversible)

%%  X1 04: streamScan . streamFilter
%% GRID CELL 4
04. \texttt{streamFilter >>> streamScan}

Same reasoning as 2.

%%  X  05: streamWindow . streamFilter
%%  X4 06: streamExpand . streamFilter
%%  X5 07: streamJoin . streamFilter
%%  X4 14: streamExpand . streamMap

%%  X1 18: streamMap . streamFilterAcc
%% GRID CELL 18
18. \texttt{streamFilterAcc >>> streamMap}

Same reasoning as 2.

%%  X  20: streamScan . streamFilterAcc
%% GRID CELL 20
20. \texttt{streamFilterAcc >>> streamScan}

Same reasoning as 2.

%%  X  21: streamWindow . streamFilterAcc
%%  X4 22: streamExpand . streamFilterAcc
%%  X  23: streamJoin . streamFilterAcc
%%  X8 24: streamMerge . streamFilterAcc

%%  X7  25: streamFilter . streamScan
%% GRID CELL 25
25. \texttt{streamScan >>> streamFilter}

We can't compose the arguments from streamScan with the predicate from streamFilter
and get the same results since we can't thread the result back in as per streamScan
(consider trying this with "counter")

%%  X9  26: streamMap . streamScan
%% GRID CELL 26
26. \texttt{streamScan >>> streamMap}

Not possible: the problem is the streamMap-arg-processed accumulator output is
fed back in

%%  X7  27: streamFilterAcc . streamScan
%% GRID CELL 27
27. \texttt{streamScan >>> streamFilterAcc}

As 25, above.

%%  X6 28: streamScan . streamScan
%% GRID CELL 28
28. \texttt{streamScan >>> streamScan}

the problem is the accumulator of scan is not hidden (like filterAcc); it's
the return value! so we can't easily hide our work

TODO this explanation is lacking

%%  X  29: streamWindow . streamScan
%%  X4 30: streamExpand . streamScan


%%  X3 38: streamExpand . streamWindow
%% GRID CELL 38
38. \texttt{streamWindow >>> streamExpand}

In terms of the value payload of a stream, an expand immediately after a window
can eliminate both operators. However, from the perspective of the Event type
that wraps the payload, \texttt{streamWindow} is a destructive operation. The additional
metadata in each Event is lost when multiple Events are aggregated into a newly
constructed Event. Therefore, this rewrite is not Total.

\begin{code}
windowExpandPre n    = streamExpand . streamWindow (chop n)
prop_windowExpand1  :: Stream Char -> Bool
-- failing!
prop_windowExpand1 s = (windowExpandPre 2) s == s

-- works but expensive to evaluate
pxxp_windowExpand2 :: Int -> Stream Char -> Bool
pxxp_windowExpand2 n s = (windowExpandPre n) s == s

-- TODO: why does prop_windowExpand1 fail if pxxp_windowExpand2 does not?
-- does pxxp_windowExpand2 2 fail? Can we write a generator that populates
-- and checks the other Event fields, so that they all fail?
--
-- Perhaps these are behaving the way they are because our Arbitrary instance
-- does generate a random ID field. The timestamp is fixed to Nothing still.
\end{code}


%%  X  39: streamJoin . streamWindow
%%  X  47: streamJoin . streamExpand
%%  X2 49: streamFilter . streamJoin
%%  X2 50: streamMap . streamJoin
%%  X2 51: streamFilterAcc . streamJoin
%%  X2 52: streamScan . streamJoin
%%  X2 53: streamWindow . streamJoin
%%  X2 54: streamExpand . streamJoin
%%  X2 55: streamJoin . streamJoin
%%  X2 56: streamMerge . streamJoin
%%  X8 59: streamFilterAcc . streamMerge
%%  X8 60: streamScan . streamMerge
%%  X? 61  streamWindow . streamMerge
%%  X  63: streamJoin . streamMerge

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\subsection{Non-total rules}

TODO explain what this means

\begin{enumerate}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GRID CELL 8
\item \texttt{streamMerge [streamFilter f s1, streamFilter f s2]
    = streamFilter f \$ streammerge [s1, s2]}

This is not "total", because ordering is not preserved.

TODO could we use the Maybe trick?

\begin{code}
filterMergePre  s  = streamMerge [streamFilter f sA, streamFilter f s]
filterMergePost s  = streamFilter f $ streamMerge [sA, s]
-- this is very slow to execute but passes
pxxp_filterMerge s = sort (filterMergePre s) == sort (filterMergePost s)
pxxp_filterMerge2 s = filterMergePre s == filterMergePost s

-- Nope! ... failing
filterMergePost2 = mergeFilterPost2
prop_filterMerge2 s = filterMergePre s == filterMergePost2 s
-- 
sX = [Event {eventId = -2, time = Nothing, value = Just 'L'},
      Event {eventId = -2, time = Nothing, value = Just '\212'}]

\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GRID CELL 48
%%  12 48: streamMerge . streamExpand
\item \texttt{streamMerge [streamExpand s2, streamExpand s2]
    = streamExpand (streamMerge [s1,s2])}

TODO not total, ordering not preserved (in some circumstances? when a stream
terminates?) remove from this section

\begin{code}
expandMergePre s = streamMerge [streamExpand w1, streamExpand w2] where
    w1 = streamWindow (chop 2) sA
    w2 = streamWindow (chop 2) s

expandMergePost s = streamExpand (streamMerge [w1,w2]) where
    w1 = streamWindow (chop 2) sA
    w2 = streamWindow (chop 2) s

-- expensive, passes
pxxp_expandMerge s = sort (expandMergePre s) == sort (expandMergePost s)
-- or
pxxp_expandMerge2 s = expandMergePre s == expandMergePost s
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  57 57: streamFilter . streamMerge

%% GRID CELL 57
\item \texttt{streamMerge >>> streamFilter}

Simply pushing the filters up to the streams that are the input to the merge
will not preserve the precise order of events. This can be addressed with the
additional Maybe wrapping and unwrapping as below

TODO can we use this Maybe trick for any of the other non-total cases around
streamMerge?

TODO pre and post are the same thing! In both cases the filter is further
downstream. Does the maybe trick still work if we fix this?

\begin{code}
mergeFilterPre  s  = streamFilter f $ streamMerge [sA, s]
-- this does not preserve order
mergeFilterPost s  = streamMerge [streamFilter f sA, streamFilter f s]
-- this is very slow to execute but passes
pxxp_mergeFilter s = sort (mergeFilterPre s) == sort (mergeFilterPost s)
-- fails due to reordering
pxxp_mergeFilter2 s =  mergeFilterPre s == mergeFilterPost s

mergeFilterPost2 s = streamMap fromJust
                   $ streamFilter isJust
                   $ streamMerge [
                       streamMap (ifJust f) sA,
                       streamMap (ifJust f)  s
                   ] where ifJust f v = if f v then Just v else Nothing

prop_mergeFilter s = mergeFilterPre s == mergeFilterPost2 s
\end{code}

There are some issues to consider about constant or variable size of
lists in the case where the stream data type is a list, such as after
a streamWindow operator. In the case of streamWindow, the output list
size will be constant, but this is not reflected in the type.
(TODO : where does this matter?)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  3 62: streamExpand . streamMerge

%% GRID CELL 62
\item \texttt{streamExpand (streamMerge [s1,s2])
    = streamMerge [streamExpand s2, streamExpand s2]}

TODO not total, reorders

\begin{code}
mergeExpandPre s = streamExpand (streamMerge [w1,w2]) where
    w1 = streamWindow (chop 2) sA
    w2 = streamWindow (chop 2) s

mergeExpandPost s = streamMerge [streamExpand w1, streamExpand w2] where
    w1 = streamWindow (chop 2) sA
    w2 = streamWindow (chop 2) s

-- \textit{very} expensive to evaluate
-- passes
pxxp_mergeExpand s = sort (mergeExpandPre s) == sort (mergeExpandPost s)
-- fails: reorders
pxxp_mergeExpand2 s = mergeExpandPre s == mergeExpandPost s
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  11 40: streamMerge . streamWindow
%% GRID CELL 40
\item \texttt{streamWindow >>> streamMerge}

TODO this is a very specific example. Can we always compose frob? Effectively
we need to know how the WindowMaker works. So really this is not a "totally
applicable" rule; we either need fission or more user-supplied information.

\begin{code}
frob :: WindowMaker a -- Stream a -> [Stream a]
frob [] = []
frob s = let [x1,y1,x2,y2,x3,y3,x4,y4,x5,y5] = take 10 s
         in [x1,x2,x3,x4,x5] : [y1,y2,y3,y4,y5] : (frob (drop 10 s))

windowMergePre s = streamMerge [ streamWindow (chop 5) sA
                               , streamWindow (chop 5) s ]

windowMergePost s = streamWindow frob (streamMerge [sA,s])

-- failing
test_windowMerge = assertBool $ take 10 (windowMergePre sB) == take 10 (windowMergePost sB)

-- TODO: doesn't work: failing on an empty list?
-- Behaviour when sampling <10 is different (due to frob impl)
prop_windowMerge s = windowMergePre s == windowMergePost s
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\end{enumerate}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Summary}

\arabic{TotalRuleCount} rules

TODO any inversions? with or without caveats? (without caveats would probably
be caught just through the systematic approach)

?? adding partial rules that do not preserve re-ordering

It appears to not be possible to perform the same promotion/fusion trick
with streamScan as streamFilterAcc (TODO: Why?)

Some rules appear to have merit for stream optimisation on their own
without considering the wider context of a stream processing graph:
e.g. filter fusion, matching a class of optimisation from the IBM paper,
and intuitively an improvement to a stream processing program (better
time/space complexity)

Others do not.

\subsection{join}

For pairs where the first operator is join, we know that the second
must operate on a tuple. However we cannot use this information to
decompose the arguments to higher order functions (filter or map), so in
general it seems no useful rewrites exist for this category of pairs.

\section{Conclusion}

There are 64 pairings of 8 functional operators. Systematically looking for
ways to rewrite each pair whilst preserving the functional definition yielded
\arabic{TotalRuleCount} rewrite rules that are applicable in any circumstance.

% TODO recalc these numbers and re-incorporate:
% 12 rules classified as applicable in any circumstance (total), a further 7 with caveats
% discovered by testing for inversions of the first 12, and a further
% 8 partial rules that apply if certain external properties of the stream
% apply (such as, strict order not being important)

These rules may prove useful as a base set of possible rewrites that
could be applied to a stream processing graph in order to change and/or
optimise the non-functional behaviour of the graph.

Examination of the rules, in particular the partial rules, has revealed
some properties of the graphs that, if encoded and provided to a rewrite
system, could aid in making more effective rewriting decisions. For
example if strict ordering of stream events is not important, then a
further 6 rewrite rules could be applied.

\subsection{Further work}

 * looking at triples or other combinations of operators
 * factoring in consideration of partitions
 * selection and encoding of additional information about streams for
   rewrite purposes

\section{References}

\bibliographystyle{abbrv}
\bibliography{references}

\end{document}
