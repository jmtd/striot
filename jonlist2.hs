{-
    simple recursive ADT to explore quickcheck's bounded
    generation

    need to consider how to handle graphs
-}
import Test.QuickCheck

data JonList a = Stop | And a (JonList a)
n & l = And n l -- cons-ish

instance Show a => Show (JonList a) where
    show Stop = "."
    show (And a l) = (show a) ++ ", " ++ (show l)

instance Arbitrary a => Arbitrary (JonList a) where
    arbitrary = sized foo

foo 0 = return Stop
foo n | n>0 = do
    v <- arbitrary
    t <- foo (n - 1)
    return (And v t)

-- length 30
testGen = generate arbitrary :: IO (JonList Int)

-- so we can use length on JonLists
instance Foldable JonList where
   foldMap f Stop = mempty
   foldMap f (And h t) = f h `mappend` foldMap f t
