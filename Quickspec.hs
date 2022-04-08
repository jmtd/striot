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

  -- this rule significantly slows down QuickSpec
  --
  {-
   -WARNING: The following types have no 'Arbitrary' instance declared.
    You will not get any variables of the following types:
  [Event Int] -> Int
  [Event Int] -> [Int]
  [Event Int] -> [[Int]]
  [Event Int] -> [[Event Int]]
   - -}
--, con "streamWindow"    (streamWindow    :: (Stream A -> [Stream A]) -> Stream A -> Stream [A])

-- including this causes either Stack Overflow or Out Of Memory on
-- my systems
--, con "streamFilterAcc" (streamFilterAcc :: (B -> A -> B) -> B -> (A -> B -> Bool) -> Stream A -> Stream A)

  {-
   -You will not get any variables of the following types:
  [Event Int] -> Int
  [Event Int] -> Bool
  [Event [Int]] -> Int
  [Event [Int]] -> Bool
  [[Event Int]] -> Int
  [[Event Int]] -> Bool
  [Event (Int, Int)] -> Int
  [Event (Int, Int)] -> Bool
   - -}

  , inst (Sub Dict :: Ord A :- Ord (Event A))
  , inst (Sub Dict :: Arbitrary A :- Arbitrary (Event A))

  , funs
  , background [ con "both"  (both :: (A -> Bool) -> (A -> Bool) -> A -> Bool) ]
  ]


both p q e = p e && q e
