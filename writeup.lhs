\input{preamble.tex}
% preamble Haskell %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\ignore{
\begin{code}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Rewrites (htf_thisModulesTests) where

import Data.List (sort)
import Data.Char (isAscii)
import Test.Framework
import Striot.FunctionalIoTtypes
import Striot.FunctionalProcessing

main = htfMain htf_thisModulesTests

-- utility functions

sA = [Event 0 Nothing (Just i)|i<-iterate next 'a']
sB = [Event 0 Nothing (Just i)|i<-iterate next '0']
sC = [Event 0 Nothing (Just i)|i<-iterate next 'A']
sW = streamWindow (chop 2) sB

-- filter predicates
f = (>= 'a')
g = (<= 'z')

-- example streamFilterAcc arguments: accept increasing Char values only
accfn1 _ v = v
acc1 = '\NUL'
pred1 = (>=)

-- avoids a situation where pred/succ will fail on the smallest/largest Enum type
next :: (Eq a, Bounded a, Enum a) => a -> a
next a = if a == maxBound then minBound else succ a
prev :: (Eq a, Bounded a, Enum a) => a -> a
prev a = if a == minBound then maxBound else pred a

\end{code}}
% Document begins %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\date{}

\begin{document}

\title{Rewrites}
\author{Jonathan Dowland
\href{mailto:jon.dowland@ncl.ac.uk}{\nolinkurl{jon.dowland@ncl.ac.uk}}}

\maketitle
\tableofcontents

\section{Abstract}

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

\section{Background}

description of striot selection of operators

A \emph{Stream} is a list of \emph{Event}s. An \emph{Event} has an Id
field and might include an iota of data, (a datum?) and a timestamp.

\begin{verbatim}
streamFilter    :: (a -> Bool) -> Stream a -> Stream a
streamMap       :: (a -> b) -> Stream a -> Stream b
streamFilterAcc :: (b -> a -> b) -> b -> (a -> b -> Bool) -> Stream a -> Stream a
streamScan      :: (b -> a -> b) -> b -> Stream a -> Stream b
streamWindow    :: Stream a -> Stream [a]
streamExpand    :: Stream [a] -> Stream a
streamMerge     :: [Stream a] -> Stream a
streamJoin      :: Stream a -> Stream b -> Stream (a,b)
\end{verbatim}

Ignoring the stream-in, stream-out parameters, the first four operators
are unary and their functional parameter operates on the payload of the
stream, and not the stream itself (the types of the HoFs do not include
Stream)

The second set of four operators operate directly on Streams.

\section{Method}

We constructed a list of operator pairs and considered each pair in
sequence in order to systematically explore all possible combinations.
For each pair, we initially attempted to reverse the order of the
operators whilst preserving the functional result of their combination
on an input stream. For pairs of the same operator, we attempt to
construct an equivalent, single, fused operator. For pairs of operators
that are the dual of one another, we attempted to eliminate them.

This method sometimes yielded a result that turned out not to behave
identically to the original pair in some or all circumstances. If we
thought the rewrite could still be useful in practise, we recorded it
with the appropriate annotation.

Once we had determined a viable rewrite rule, we examined whether its
inverse was a second, distinct viable rule.

\subsection{Semantic analysis}

\texttt{streamMap} and \texttt{streamFilter} are stream variants of the
widely used \texttt{map} and \texttt{filter} which have well understood
semantics.

\texttt{streamWindow}, \texttt{streamExpand} and \texttt{streamJoin}
manipulate the structure of the stream itself: \texttt{streamWindow}
eliminates the Event wrappers around the iotas that flow into it and
synthesises new ones for the collected iotes it outputs.
\texttt{streamExpand} is its dual. \texttt{streamJoin} eliminates two
\texttt{Event} wrapper and synthesises a new one containing a tuple.

For this reason, the following rewrite rule is not applicable in all
cases:

\begin{verbatim}
streamExpand . streamWindow _ = id
\end{verbatim}

This holds only in terms of the data encapsulated within the stream. But
since \texttt{streamWindow} discards the \texttt{Event} wrappers around
the iotas flowing into it, we lose the information contained therein,
including the timestamp field.

The type of \texttt{streamMerge} indicates that it could manipulate the
input \emph{Event}s, but we know in practise that it doesn't.

\subsection{Tools}

Most of this exercise was conducted offline using pen and paper. For
some of the more complex rewrites, we sketched an outline on paper and
then attempted to encode the result in Haskell. Two Haskell libraries in
particular proved very useful for testing our work:
QuickCheck\textless{}\textgreater{} and the Haskell Testing
Framework\textless{}\textgreater{}. Using these tools, we attempted to
construct a ``property'' that expressed the equivalence of the operator
pair before and after our rewrite. QuickCheck then generates a set of
randomly generated inputs and compares the output of each side of the
equation.

This provided some confidence that each rewrite rule was correct. It
also helped to catch a few cases where we had authored a rewrite rule we
thought was total, but wasn't. We then explored the failure cases to
determine whether the rule needed to be thrown out, or was useful in
some circumstances, in which case we noted the necessary caveats.

XXX ``total'' mentioned in the para above; but defined below

In order for QuickCheck to generate random Stream data, we were required
to provide a trivial implementation of the Arbitrary class for the
appropriate data type in STRIoT. This data type is higher-order, and so
to evaluate the properties, we needed to bind the type variables to
something concrete. We chose \texttt{Char}, which has several nice
properties: it satisfies \texttt{Eq}, permitting the comparison of
streams of \texttt{Chars}; it satisfies \texttt{Ord}, simplifying the
writing of predicates for \texttt{streamFilter} and friends (we can use
standard functions including \texttt{\textgreater{}=} and
\texttt{succ}). Finally, when examining the output of operator-pairs in
an interactive session, \texttt{Char} is quite friendly to human eyes.

\subsection{Describing rewrites}

\subsubsection{total/partial}

We describe a rewrite as total if it can be applied to any occurance of
the pattern. Some rewrites are only applicable if some other properties
of the stream hold. These properties are not expressed in the definition
or type of the rewrite function. Indeed they may not be expressible due
to the limits of the type system.

An example of a partial rewrite rule might be one that does not
guarantee that the ordering of the input Events is preserved in the
rewritten version. Another is the elimination of adjacent window/expand
operations, which result in the loss of the timestamp and id metadata
from the input Events. In both cases whether these are important
considerations is application-specific.

\section{TODO}

Classification of non-order-preserving rewrites: whether the re-ordering
is determined \emph{internally} or \emph{externally}. Internally means
that the re-ordering is entirely determined by the composition of stream
operators; external means that it is dependent on one of the externally
supplied arguments, such as the predicate supplied to streamFilter.

\pagebreak
\section{Results}

Each rule is described in Haskell code. The rule is referred to in the
short hand by the name of the operators concerned. So, for filter into
filter, The short-hand is "filterFilter". The left-hand side of the
rule is encoded as a function with the suffix "Pre", and the right hand
with the suffix "Post". One or more test functions are defined to
provide assurance that the two sides are equivalent. those whose names
are prefixed with "prop\_" are properties that are exercised by QuickCheck.
Those whose names are prefixed with "test\_" are more simple boolean
tests.

\begin{enumerate}
 % 1
 \item \lstinline|streamFilter f . streamFilter g = streamFilter (\x -> f x && g x)|
    total; fusion

\begin{code}
filterFilterPre     = streamFilter g . streamFilter f
filterFilterPost    = streamFilter (\x -> f x && g x)
prop_filterFilter s = filterFilterPre s == filterFilterPost s
\end{code}

 % 2
 \item \lstinline|streamFilter| and \lstinline|streamFilterAcc| fusion
 p2 . streamFilterAcc accfn acc p1 
     = streamFilterAcc accfn1 acc1 (\x a -> p1 x a && p2 x)|
    total; fusion

\begin{code}

filterFilterAccPre     = streamFilterAcc accfn1 acc1 pred1 . streamFilter g
filterFilterAccPost    = streamFilterAcc
    (\a v -> if g v then accfn1 a v else a)
    acc1
    (\x a -> g x && pred1 x a)

prop_filterFilterAcc s = filterFilterAccPre s == filterFilterAccPost s

\end{code}
\end{enumerate}

\begin{verbatim}

 3. `streamFilterAcc accfn acc p2 . streamFilter p1 =
     = streamFilterAcc (\a v -> if p1 v then accfn a v else a)
     acc
     (\x a -> p1 x && p2 x a)`
    total; fusion

 4. `streamFilterAcc accfn2 acc2 p2 . streamFilterAcc accfn1 acc1 p1
     = streamFilterAcc
       (\(x,y) v -> (accfn1 x v, if p1 v x then accfn2 y v else y))
       (acc1,acc2)
       (\x (y,z) -> p1 x y && p2 x z)`
    total; fusion

 5. `streamFilter p . streamMap f = streamMap f . streamFilter (p . f)`
    total.
    efficiency of RHS:
    If p is highly selective, then the overhead of evaluating f
    twice per selected event may be lower than the savings made by
    reducing the list de/reconstruction overhead of streamMap.

 6. `streamMap f . streamMap g = streamMap (f . g)`
    total; fusion

 7. `streamJoin s1 . streamMap f = streamMap (\(x,y) -> (x, f y)) .  streamJoin s1`
    total

8. `streamFilter f . streamExpand = streamExpand . streamMap (filter f)`
    total.

9. `streamMap f . streamExpand = streamExpand . streamMap (map f)`
    total.

10. `streamMap f $ streamMerge [s1, s2]
    = streamMerge [streamMap f s0, streamMap f s2]`
    total.

11. `streamMerge [s1, streamMerge [s2, s3]]
    = streamMerge [s0, s2, s3]`
    total
    ordering preserved in the right-associative case

12. `streamFilterAcc af a p . streamMap f
    = streamMap f . streamFilterAcc af a (p . f)`
    total

13. `streamScan accfn acc . streamMap f
    = streamScan (flip (flip accfn . f)) acc`
    total; fusion
\end{verbatim}

\subsection{Inverted rules}

When the above rules were derived, each was analysed to determine
whether it could be inverted: whether any occurence of a stream matching
the pattern on the right could be replaced with that on the left.

The following rules are inverted versions of the above, but are subject
to a \emph{decomposition caveat}: The arguments to the stream operators
on the left hand side of these rules are \emph{compound expressions},
that are decomposed and their constituent expressions used on the
right-hand side.

When we are considering a practical system of applying such rules to a
Stream Graph, it is unlikely that we are going to be able to decompose
or inspect the composition of the functional arguments, so these rules
may be of limited practical use.

{[}start=14{]} 14.
\texttt{filter\ (\textbackslash{}x\ -\textgreater{}\ f\ x\ \&\&\ g\ x)\ =\ filter\ f\ .\ filter\ g}

\begin{verbatim}
15. `streamMap f . streamFilter (p . f) = streamFilter p . streamMap f`

16. `streamMap (f . g) = streamMap f . streamMap g`

17. `streamMap (\(x,y) -> (x, f y)) . streamJoin s1 = streamJoin s1 . streamMap f`

18. `streamExpand . streamMap (filter f) = streamFilter f .  streamExpand`
    (XXX it would be good to write QuickCheck properties for the inversions)

19. `streamExpand . streamMap (map f) = streamMap f . streamExpand`

20. `streamMap f . streamFilterAcc af a (p . f)
    = streamFilterAcc af a p . streamMap f`
\end{verbatim}

\subsubsection{inverted \texttt{streamMerge} rules}

The semantics of \texttt{streamMerge} are unique amongst the stream
operators, given its unique type signature.

XXX expand

{[}start=21{]} 21.
\texttt{streamMerge\ {[}streamMap\ f\ s1,\ streamMap\ f\ s2{]}\ \ \ \ \ \ \ \ \ =\ streamMap\ f\ \$\ streamMerge\ {[}s-1,\ s2{]}}

\begin{verbatim}
XXX this is kind of a special case of the composition caveat?

22. `streamMerge [s1, s2, s3]
    = streamMerge [s1, streamMerge [s2, s3]]`
\end{verbatim}

\subsection{Partial rules}

The following rules do not preserve the metadata contained within the
Event structures. XXX what do they do to ``empty'' events? I.e. Nothing
instead of a datum? are they discarded in the window function?

{[}start=23{]} 23. \texttt{streamExpand\ .\ streamWindow\ \_\ =\ id}

\begin{verbatim}
 24. `streamWindow w . streamMap f = streamMap (map f) . streamWindow w`
    
    only works if streamWindow predicate does not look at value:
    window (>=3) . map (+1) [1,2,3,4] /= map (+1) . window (>=3) [1,2,3,4]
    otoh that's not a valid windowmaker either.
\end{verbatim}

The following partial rules do not preserve the order of stream events:

{[}start=25{]} 25.
\texttt{streamMerge\ {[}streamExpand\ s1,\ streamExpand\ s2{]}\ \ \ \ \ \ \ \ \ =\ streamExpand\ (streamMerge\ {[}s2,s2{]})}

\begin{verbatim}
26. `streamMerge [streamFilter f s1, streamFilter f s2]
    = streamFilter f $ streammerge [s1, s2]`
\end{verbatim}

and their inverses

{[}start=27{]} 27.
\texttt{streamExpand\ (streamMerge\ {[}s1,s2{]})\ \ \ \ \ \ \ \ \ =\ streamMerge\ {[}streamExpand\ s2,\ streamExpand\ s2{]}}

\begin{verbatim}
28. `streamFilter f $ streammerge [s1, s2]
    = streamMerge [streamFilter f s1, streamFilter f s2]`
\end{verbatim}

There are some issues to consider about constant or variable size of
lists in the case where the stream data type is a list, such as after a
streamWindow operator. In the case of streamWindow, the output list size
will be constant, but this is not reflected in the type. (XXX: where
does this matter?)

\subsection{Summary}

15 rules 23 adding inversions (with caveats) 28 adding partial rules
that do not preserve re-ordering

It appears to not be possible to perform the same promotion/fusion trick
with streamScan as streamFilterAcc (XXX: Why?)

\subsection{join}

For pairs where the first operator is join, we know that the second must
operate on a tuple. However we cannot use this information to decompose
the arguments to higher order functions (filter or map), so in general
it seems no useful rewrites exist for this category of pairs.

\section{Conclusion}

There are 64 pairings of 8 functional operators. Systematically looking
for ways to rewrite each pair whilst preserving the functional
definition yielded up to 27 rewrite rules: 12 rules classified as
applicable in any circumstance (total), a further 7 with caveats
discovered by testing for inversions of the first 12, and a further 8
partial rules that apply if certain external properties of the stream
apply (such as, strict order not being important)

These rules may prove useful as a base set of possible rewrites that
could be applied to a stream processing graph in order to change and/or
optimise the non-functional behaviour of the graph.

Examination of the rules, in particular the partial rules, has revealed
some properties of the graphs that, if encoded and provided to a rewrite
system, could aid in making more effective rewriting decisions. For
example if strict ordering of stream events is not important, then a
further 6 rewrite rules could be applied.

\subsection{Further work}

\begin{itemize}
\tightlist
\item
  looking at triples or other combinations of operators
\item
  factoring in consideration of partitions
\item
  selection and encoding of additional information about streams for
  rewrite purposes
\end{itemize}

{[}bibliography{]} == References

\begin{itemize}
\tightlist
\item
  {[}{[}{[}QuickCheck{]}{]}{]}
\item
  {[}{[}{[}HTF{]}{]}{]}
\end{itemize}

\end{document}
