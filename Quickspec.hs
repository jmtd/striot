{-# LANGUAGE ScopedTypeVariables, ConstraintKinds, RankNTypes, ConstraintKinds, FlexibleContexts, TypeOperators #-}

import QuickSpec
import Test.QuickCheck
import Striot.FunctionalIoTtypes
import Striot.FunctionalProcessing

main = quickSpec
  [ con "streamFilter"    (streamFilter    :: (A -> Bool) -> Stream A -> Stream A)
  , con "streamMap"       (streamMap       :: (A -> B) -> Stream A -> Stream B)
  , con "streamScan"      (streamScan      :: (B -> A -> B) -> B -> Stream A -> Stream B)
  , con "streamMerge"     (streamMerge     :: [Stream A] -> Stream A)
  , con "streamExpand"    (streamExpand    :: Stream [A] -> Stream A)
  , con "streamJoin"      (streamJoin      :: Stream A -> Stream B -> Stream ( A,B))
  , con "streamWindow"    (streamWindow    :: (Stream A -> [Stream A]) -> Stream A -> Stream [A])
  , con "streamFilterAcc" (streamFilterAcc :: (B -> A -> B) -> B -> (A -> B -> Bool) -> Stream A -> Stream A)

  , inst (Sub Dict :: Ord A :- Ord (Event A))
  , inst (Sub Dict :: Arbitrary A :- Arbitrary (Event A))
  ]
