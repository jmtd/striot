{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Applicative
import Data.Function ((&))
import Striot.FunctionalProcessing
import Striot.FunctionalIoTtypes

type StrExp a = Code Q a
type StreamProg a = StreamProgram (Stream a)

data StreamProgram o where
  StreamSource    :: StrExp o -> StreamProg o
  StreamSink      :: StrExp (i -> o) -> StreamProg i -> StreamProgram o
  StreamMap       :: StrExp (i -> o) -> StreamProg i -> StreamProg o
  StreamScan      :: StrExp (o -> a -> o) -> StrExp o -> StreamProg a -> StreamProg o
  StreamFilter    :: StrExp (i -> Bool) -> StreamProg i -> StreamProg i
  StreamFilterAcc :: StrExp (b -> i -> b) -> StrExp b -> StrExp (i -> b -> Bool) -> StreamProg i -> StreamProg i
  StreamWindow    :: StrExp (Stream o -> [Stream o]) -> StreamProg o -> StreamProg [o]
  StreamExpand    :: StreamProg [o] -> StreamProg o
  StreamMerge     :: [StreamProg o] -> StreamProg o
  StreamJoin      :: StreamProg a -> StreamProg b -> StreamProg (a,b)

test1 :: StreamProgram (IO ())
test1 = StreamSource [|| 3 ||]
      & StreamFilter [|| (>5) ||]
      & StreamFilter [|| (<8) ||]
      & StreamWindow [|| chop 1 ||]
      & StreamSink   [|| print ||]

test2 :: StreamProg Integer -> StreamProgram (IO ())
test2 = StreamSink   [|| print ||]
      . StreamWindow [|| chop 1 ||]
      . StreamFilter [|| (<8) ||]
      . StreamFilter [|| (>5) ||]


-- example instances for StrExp
e1 :: StrExp (Int -> Int)
e1 = [|| (+3) ||]
e2 :: StrExp (Int -> Int)
e2 = [|| (\n -> n-2) ||]

printProgram :: StreamProgram a -> IO ()
printProgram = mapM_ print . reverse . doSomething

doSomething :: StreamProgram a -> [String]
doSomething (StreamSource fn)          = "the source!" : []
doSomething (StreamMap fn parent)      = "map"         : doSomething parent
doSomething (StreamFilter pred parent) = "filter"      : doSomething parent
doSomething (StreamSink fn parent)     = "the sink!"   : doSomething parent


-- rewrite rules

-- lifting composition into StrExp / Code Q is achieved with typed splices
-- and types quasi-quotes
expCmp :: StrExp (b -> c) -> StrExp (a -> b) -> StrExp (a -> c)
expCmp e1 e2 = [|| $$(e1) . $$(e2) ||]

-- StreamMap f . streamMap g = streamMap (f . g)
mapFuse :: StreamProgram o -> Maybe (StreamProgram o)
mapFuse (StreamMap f (StreamMap g instream)) = Just $
    StreamMap (expCmp f g) instream
mapFuse _ = Nothing

mapFuse' :: StreamProgram o -> Maybe (StreamProgram o)
mapFuse' (StreamMap f (StreamMap g instream)) = Just $
    StreamMap [|| $$(f) . $$(g) ||] instream
mapFuse' _ = Nothing

-- applying rewrite rules
-- we would need to visit every node in a program to find if a rule
-- could be applied, and if so, potentially change the structure of
-- the whole program. Functor is not suitable due to the latter.
-- Foldable perhaps?
--
{-
instance Foldable (StreamProgram a) where
    foldl :: (b -> a -> b) -> b -> StreamProgram a -> b
    foldl f acc (StreamSource e) = f acc $$(e)
    -- ‘e’ is used in a top-level splice, quasi-quote, or annotation,
    -- and must be imported, not defined locally
 -}

{-
-- without something like foldl (above), we would need to pattern-match
-- every constructor. Note we're fucked on type here; we can't put the
-- two parents of streamJoin in the same list because they're different
-- types;
graphParents :: Streamprogram a -> [StreamProgram b]
graphParents (StreamSource _) = Nothing

  (StreamSink _ p) = p
  (StreamMap _ p) = p
  (StreamScan _ _ p) = p
  (StreamFilter _ p) = p
  (StreamFilterAcc _ _ _ p) = p
  (StreamWindow _ p) = p
  (StreamExpand   )
  (StreamMerge    )
  (StreamJoin     )
-}

{-
applyRule :: (StreamProgram a -> Maybe (StreamProgram b)) -> StreamProgram c -> StreamProgram c -- XXX: it might change the type?!
applyRule r p = case r p of
    Nothing -> p
    Just p' -> p'
-- oh god some complex type stuff to understand here. the above fails because a,b,c
-- are universally quantified. IOW the compiler cannot reconcile StreamProgram b and
-- StreamProgram c, so 'r p' is not acceptable
 - -}

-- what about Eq?
instance Eq (StreamProgram o) where
    (StreamSink exp1 par1) == (StreamSink exp2 par2) =
        exp1 == exp2 && par1 == par2
        -- GHC can't unify the types in StrExp here
