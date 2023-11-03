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
mapFuse :: StreamProgram o -> StreamProgram o
mapFuse (StreamMap f (StreamMap g instream)) =
    StreamMap (expCmp f g) instream

mapFuse' :: StreamProgram o -> StreamProgram o
mapFuse' (StreamMap f (StreamMap g instream)) =
    StreamMap [|| $$(f) . $$(g) ||] instream
