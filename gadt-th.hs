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


data StreamProgram o where
    StreamSource :: Code Q o                                       -> StreamProgram (Stream o)
    StreamMap    :: Code Q (i -> o)    -> StreamProgram (Stream i) -> StreamProgram (Stream o)
    StreamFilter :: Code Q (i -> Bool) -> StreamProgram (Stream i) -> StreamProgram (Stream i)
    StreamSink   :: Code Q (i -> o)    -> StreamProgram (Stream i) -> StreamProgram (o)
    StreamExpand :: StreamProgram (Stream [o])                     -> StreamProgram (Stream o)

{-
(fmap unType . examineCode) [|| 3::Int ||]
SigE (LitE (IntegerL 3)) (ConT GHC.Types.Int)
 -}

printProgram :: StreamProgram a -> IO ()
printProgram = mapM_ print . reverse . doSomething

doSomething :: StreamProgram a -> [String]
doSomething (StreamSource fn)          = "the source!" : []
doSomething (StreamMap fn parent)      = "map"         : doSomething parent
doSomething (StreamFilter pred parent) = "filter"      : doSomething parent
doSomething (StreamSink fn parent)     = "the sink!"   : doSomething parent
