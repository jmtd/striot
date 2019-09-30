{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}


import Striot.FunctionalProcessing
import Striot.FunctionalIoTtypes
import Language.Haskell.TH
import Language.Haskell.TH.Syntax -- (Quasi, lift…)

foo = streamMap (+1)
bar = streamMap (+2)
baz = foo . bar

huh = [| baz |] -- : Language.Haskell.TH.Lib.Internal.ExpQ
-- lovely.
-- needs passing through runQ get a bare exp "VarE Main.baz"


-- we want to write a function that takes an argument that is
-- a stream processing thing (type Stream a, perhaps) and looks
-- at it to determine if it's two maps, say, that we can fuse

-- how to "visualise" ExpQs?

------------------------------------------------------------------------------
-- a function that can identify whether there's a "streamMap" embedded
-- in the supplied expression
-- 

-- unused
findMap :: Lift a => Stream a -> IO Bool
findMap str = do
    runQ [| str |] >>= print
    exp <- runQ [| str |]
    return $ findMap' exp

findMap' exp = case exp of
        ListE [] -> False
        ListE xs -> or $ map findMap' xs 
        AppE a b -> findMap' a || findMap' b
        VarE n   -> (show n) == "Striot.FunctionalProcessing.streamMap"
        _        -> False

-- permits using in the style of blah, below
findMap'' exp = return $ findMap' exp

streamSrc = map (\n -> (Event 0 Nothing (Just n))) [1,2,4,5]

blah :: Quasi m => m Bool
blah = runQ [| streamMap (+1) streamSrc |] >>= findMap'' -- True

-- temporarily ignore the time argument to avoid having to implement Lift for
-- UTCTime and all constituent types
instance Lift a => Lift (Event a) where
    -- lift :: lift t => t -> Q exp
    lift (Event i t v) = do
        i' <- runQ (lift i)
        v' <- runQ (lift v)
        let t' = ListE [] -- dummy value
        return $ AppE (AppE (AppE (ConE (mkName "Event")) i') t') v'
