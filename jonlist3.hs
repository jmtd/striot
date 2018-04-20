{-
    simple recursive ADT to explore quickcheck's bounded
    generation

    need to consider how to handle graphs
-}
import Test.QuickCheck
import Control.Monad

data Tree = Leaf Int | Branch Tree Tree deriving (Show)

instance Arbitrary Tree where
    arbitrary = sized foo

foo 0 = do
    i <- arbitrary
    return $ Leaf i
foo n | n>0 = do
    l <- foo (n `div` 2)
    r <- foo (n `div` 2)
    return $ Branch l r

-- length 30
testGen = generate arbitrary :: IO Tree
