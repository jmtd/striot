{-
    simple recursive ADT to explore quickcheck's bounded
    generation

    need to consider how to handle graphs
-}
import Test.QuickCheck

data JonList a = Stop | And a (JonList a)

instance Show a => Show (JonList a) where
    show Stop = "."
    show (And a l) = (show a) ++ ", " ++ (show l)

instance Arbitrary a => Arbitrary (JonList a) where
    arbitrary = oneof [ return Stop
                      , do
                          v <- arbitrary
                          t <- arbitrary
                          return (And v t)
                      ]


-- up to 30 in length but 50/50 chance of terminating at each point
testGen = generate arbitrary :: IO (JonList Int)


