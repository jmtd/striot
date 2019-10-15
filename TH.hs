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

findMap expq = runQ expq >>= return . findMap'

-- TODO rather than exhaustively pattern-match through all Exp types (and
-- also Pattern types, and Dec types, and…), explore "generic traversal",
-- e.g. https://michaeldadams.org/papers/tyb/tyb-2012-haskell.pdf via
-- https://www.microsoft.com/en-us/research/wp-content/uploads/2003/01/hmap.pdf
findMap' exp = case exp of
        ListE [] -> False
        ListE xs -> or $ map findMap' xs 

        AppE (VarE n) b -> (show n)
            == "Striot.FunctionalProcessing.streamMap" || findMap' b

        AppE a b -> findMap' a || findMap' b

        InfixE (Just a) b (Just c) -> findMap' a || findMap' b || findMap' c
        InfixE Nothing  b (Just c) -> findMap' b || findMap' c
        InfixE (Just a) b Nothing  -> findMap' a || findMap' b
        InfixE Nothing  b Nothing  -> findMap' b

        LamE [p] e -> findMap' e -- XXX no need to look at the patterns?
        CondE a b c -> findMap' a || findMap' b || findMap' c
        TupE xs  -> or $ map findMap' xs

        LetE decs e -> findMap' e
        _        -> False

testMap expq = assertBool =<< findMap expq

-- passing tests
test_bare   = testMap [| streamMap (+1) streamSrc |]
test_two    = testMap [| streamMap (+1) (streamMap (*2) streamSrc) |]
test_comp   = testMap [| (streamMap (+1) . streamMap (*2)) streamSrc |]
test_dollar = testMap [| streamMap (+1) . streamMap (*2) $ streamSrc |]
test_lambda = testMap [| \e -> streamMap (+1) e |]
test_cond   = testMap [| if True then streamMap (+1) else streamfilter (<1) |]
test_tuple  = testMap [| (streamMap (+1), streamFilter) |]
test_let    = testMap [| let f = streamSrc in streamMap (+1) f |]

-- failing tests
-- demonstrates we need to handle the [Dec] parameter to LetE
test_let2   = assertBool =<< findMap [| let f = streamMap in f (+1) streamSrc |]
-- demonstrate limitation of our let handling
test_let3   = testMap [| let streamMap = streamFilter in streamMap (>1) streamSrc |]
