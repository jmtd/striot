{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}

-- not sure necessary
{- LANGUAGE DeriveLift -}
{- LANGUAGE OverloadedStrings -}

import Striot.FunctionalProcessing
import Striot.FunctionalIoTtypes
import Language.Haskell.TH
import Language.Haskell.TH.Syntax -- (Quasi, lift…)
import Test.Framework

------------------------------------------------------------------------------
-- a function that can identify whether there's a "streamMap" embedded
-- in the supplied expression
-- 

-- e.g. findMap [| streamMap (+3) |]
-- substituting IO for Quasi works
--findMap :: Quasi m => Q Exp -> m Bool
findMap expq = runQ expq >>= return . findMap'

findMap' exp = case exp of
        ListE [] -> False
        ListE xs -> or $ map findMap' xs 
        AppE a b -> findMap' a || findMap' b
        VarE n   -> (show n) == "Striot.FunctionalProcessing.streamMap"
        _        -> False

-- permits using in the style of blah, below
findMap'' exp = return $ findMap' exp

-- an example of returning the actual input code
-- this doesn't work because: • GHC stage restriction:
--   ‘exp’ is used in a top-level splice, quasi-quote, or annotation,
--   and must be imported, not defined locally
{-
findMap''' expq = do
    exp <- runQ expq
    return $ if   findMap' exp
             then $( exp )
             else streamSrc
-}

streamSrc = map (\n -> (Event 0 Nothing (Just n))) [1,2,4,5]

blah :: Quasi m => m Bool
blah = runQ [| streamMap (+1) streamSrc |] >>= findMap'' -- True

blah' = findMap [| streamMap (+1) streamSrc |] :: IO Bool

test_blah = blah >>= assertBool
test_blah' = blah' >>= assertBool
test_blah'' = findMap [| streamMap (+1) streamSrc |] >>= assertBool

main = htfMain htf_thisModulesTests
