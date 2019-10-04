{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}

import Striot.FunctionalProcessing
import Striot.FunctionalIoTtypes
import Language.Haskell.TH
import Language.Haskell.TH.Syntax -- (Quasi, lift…)
import Test.Framework

------------------------------------------------------------------------------
-- boilerplate

main      = htfMain htf_thisModulesTests
streamSrc = map (\n -> (Event 0 Nothing (Just n))) [1,2,4,5]

------------------------------------------------------------------------------
-- a function that can identify whether there's a "streamMap" embedded
-- in the supplied expression

-- e.g. findMap [| streamMap (+3) |]
findMap expq = runQ expq >>= return . findMap'

-- XXX I wonder if there's a smarter way to traverse the Exp structure than
-- pattern matching like this (something like generic traversal from that
--  paper)
findMap' exp = case exp of
        ListE [] -> False
        ListE xs -> or $ map findMap' xs 
        AppE a b -> findMap' a || findMap' b
        VarE n   -> (show n) == "Striot.FunctionalProcessing.streamMap"

        InfixE (Just a) b (Just c) -> findMap' a || findMap' b || findMap' c
        InfixE Nothing  b (Just c) -> findMap' b || findMap' c
        InfixE (Just a) b Nothing  -> findMap' a || findMap' b
        InfixE Nothing  b Nothing  -> findMap' b

        LamE [p] e -> findMap' e -- XXX no need to look at the patterns?

        CondE a b c -> findMap' a || findMap' b || findMap' c
        _        -> False


test_blah  = assertBool =<< findMap [| streamMap (+1) streamSrc |]
test_blah2 = assertBool =<< findMap [| streamMap (+1) (streamMap (*2) streamSrc) |]
test_blah3 = assertBool =<< findMap [| (streamMap (+1) . streamMap (*2)) streamSrc |]
test_blah4 = assertBool =<< findMap [| streamMap (+1) . streamMap (*2) $ streamSrc |]

test_lambda= assertBool =<< findMap [| \e -> streamMap (+1) e |]

test_cond  = assertBool =<< findMap [| if True then streamMap (+1) else streamfilter (<1) |]


------------------------------------------------------------------------------
-- splicing

{-
-- an example of returning the actual input code
-- this doesn't work because: • GHC stage restriction:
--   ‘exp’ is used in a top-level splice, quasi-quote, or annotation,
--   and must be imported, not defined locally
findMap''' expq = do
    exp <- runQ expq
    return $ if   findMap' exp
             then $( exp )
             else streamSrc
-}
