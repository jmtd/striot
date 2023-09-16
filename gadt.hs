{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import Data.Function ((&))

data StreamProgram o where
    StreamSource :: Quote m => Code m o                                   -> StreamProgram (m o)
    StreamMap    :: Quote m => Code m (i -> o)    -> StreamProgram (m i)  -> StreamProgram (m o)
    StreamFilter :: Quote m => Code m (i -> Bool) -> StreamProgram (m i)  -> StreamProgram (m i)
    StreamSink   :: Quote m => Code m (i -> o)    -> StreamProgram (m i)  -> StreamProgram (m o)

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
