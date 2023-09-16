{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import Data.Function ((&))
import           Data.Time    (UTCTime)

data Event a = Event { time    :: Maybe Timestamp
                         , value   :: Maybe a}
     deriving (Eq, Ord, Show, Read)
type Stream a = [Event a]
type Timestamp       = UTCTime

type Selectivity = Double
type ArrivalRate = Double

data StreamProgram o where
    StreamSource    :: Quote m => ArrivalRate -> m Exp ->                                           StreamProgram (Stream o)
    StreamMap       :: Quote m => m Exp -> StreamProgram (Stream i) ->                              StreamProgram (Stream o)
    StreamScan      :: Quote m => m Exp -> o -> StreamProgram (Stream i) ->                         StreamProgram (Stream o)
    StreamFilter    :: Quote m => Selectivity -> m Exp -> StreamProgram (Stream i) ->               StreamProgram (Stream i)
    StreamFilterAcc :: Quote m => Selectivity -> m Exp -> o -> m Exp -> StreamProgram (Stream o) -> StreamProgram (Stream o)
    StreamMerge     ::            [StreamProgram (Stream i)] ->                                     StreamProgram (Stream i)
    StreamJoin      ::            StreamProgram (Stream a) -> StreamProgram (Stream b) ->           StreamProgram (Stream (a,b))
    StreamWindow    :: Quote m => m Exp -> StreamProgram (Stream a) ->                              StreamProgram (Stream [a])
    StreamExpand    ::            StreamProgram (Stream [o]) ->                                     StreamProgram (Stream o)
    StreamSink      :: Quote m => m Exp -> StreamProgram (Stream i) ->                              StreamProgram (Stream o)

