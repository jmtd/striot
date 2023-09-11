{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import Data.Function ((&))

data StreamProgram i o where
    StreamSource :: Quote m => Code m o                                     -> StreamProgram ()    (m o)
    StreamMap    :: Quote m => Code m (i -> o) -> StreamProgram x (m i)     -> StreamProgram i     (m o)
    StreamFilter ::            (i -> Bool)                                  -> StreamProgram i     i
    --  for StreamSink, the connecting StreamProgram needs to output its input type; but
    --  we don't care what that program's *input* type is. 'x' is used as a "don't care".
    StreamSink   :: Quote m => Code m o        -> StreamProgram x (m i)     -> StreamProgram i (m o)

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
