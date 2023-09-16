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
    StreamSource :: Quote m => m Exp                         -> StreamProgram o
    StreamMap    :: Quote m => m Exp -> StreamProgram (m i)  -> StreamProgram o
    StreamFilter :: Quote m => m Exp -> StreamProgram (m i)  -> StreamProgram i
    StreamSink   :: Quote m => m Exp -> StreamProgram (m i)  -> StreamProgram o

-- we're not yet wrapping the types in 'Stream'


{-
λ> :t StreamSource [|| 3::Int ||] & StreamSink [|| mapM_ print ||]
StreamSource [|| 3::Int ||] & StreamSink [|| mapM_ print ||]
  :: (Quote m, Foldable t, Show a) =>
     StreamProgram Int (m (t a -> IO ()))
 -}

{-
(fmap unType . examineCode) [|| 3::Int ||]
SigE (LitE (IntegerL 3)) (ConT GHC.Types.Int)
 -}
