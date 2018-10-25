-- Jon's attempt to use this stuff for stream rewrites
-- temp hacky copy to explore possible rewrites for Taxi

module MyMain where

import Expressions
import Parsing
import Laws
import Calculations

import Control.Monad (liftM2)


-- hack to encapsulate (\f g x -> f x && g x)
-- XXX more descriptive name
zomg f g x = f x && g x

-- XXX: encode the remaining transformations in particular streamJoin and streamFilterAcc transformations
mylaws = map (parse law) [ "defn filterfuse: streamFilter f . streamFilter g = streamFilter (zomg f g)"
                         , "defn mapfuse:    streamMap f . streamMap g = streamMap (f . g)"
                         , "defn mapFilter:  streamFilter p . streamMap f = streamMap f . streamFilter (p . f)"
                         , "defn mapWindow:  streamWindow w . streamMap f = streamMap (map f) . streamWindow w"

                         -- XXX this rule is only useful if it opens up other possibilies, i.e., we can fuse
                         -- two adjacent streamFilterAcc afterwards. I'd also like more assurance that it is
                         -- correct (this is an anon-lambda-free translation of the original version)
                         , "defn promote:    streamFilter p = streamFilterAcc (flip const) p (p . const)"
                         ]

taxiQ1 = parse expr "streamFilterAcc accfn acc p2 . streamMap f . streamWindow w . streamFilter p . streamMap f2"
foo7 = calculate mylaws taxiQ1

-- decomposed representation of the streamgraph for taxiQ2. The representation of the duplicate streams into
-- the join is troublesome
taxiQ2 = parse expr "streamFilterAcc a b p1 . streamMap f1 . streamWindow w1 \
                        \ . streamMap f2 . streamJoin \
                                            \ (streamFilter p2 . streamMap f3) \
                                            \ (streamFilter p2 . streamMap f3)"

foo8 = calculate mylaws taxiQ2
