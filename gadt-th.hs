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

{- If we do not include m in the output type, it is awkward to use. With output type
 - StreamProgram (o):
  λ> :t StreamSource ([|| 3::Int ||]::Code Q Int)
  StreamSource ([|| 3::Int ||]::Code Q Int) :: StreamProgram Int
  λ> :t StreamSource ([|| 3::Int ||]::Code Q Int)
  StreamSource ([|| 3::Int ||]::Code Q Int) :: StreamProgram Int
  λ> :t StreamSink ([|| print ||]:: Show a=>Code Q (a -> IO ()))
  StreamSink ([|| print ||]:: Show a=>Code Q (a -> IO ()))
    :: Show i => StreamProgram i -> StreamProgram (IO ())
  λ> :t StreamSource ([|| 3::Int ||]::Code Q Int) & StreamSink ([|| print ||]:: Show a=>Code Q (a -> IO ()))
  StreamSource ([|| 3::Int ||]::Code Q Int) & StreamSink ([|| print ||]:: Show a=>Code Q (a -> IO ()))
    :: StreamProgram (IO ())
 - But
   λ> :t StreamSource ([|| 3::Int ||])
  <interactive>:1:1: error:
      • Ambiguous type variable ‘m0’ arising from a use of ‘StreamSource’
 - if we instead  push m to the output type (StreamProgram (m o)):

  λ> :t StreamSource ([|| 3::Int ||])
  StreamSource ([|| 3::Int ||]) :: Quote m => StreamProgram (m Int)
  *Main
  λ> :t StreamSink [|| print ||]
  StreamSink [|| print ||]
    :: (Quote m, Show i) =>
       StreamProgram (m i) -> StreamProgram (m (IO ()))
  *Main
  λ> :t StreamSource ([|| 3::Int ||]) & StreamSink [|| print ||]
  StreamSource ([|| 3::Int ||]) & StreamSink [|| print ||]
    :: Quote m => StreamProgram (m (IO ()))

 -}

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
