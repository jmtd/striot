{-
    simple recursive ADT to explore quickcheck's bounded
    generation

    need to consider how to handle graphs

    Trees are easier because they expand only, never contract
    to generate graphs that might contract (join, merge), maybe
    generate a backwards one (so diverges instead) and reverse
    it?
-}
import Test.QuickCheck
import Control.Monad
import Algebra.Graph
import Algebra.Graph.Export.Dot

type JonGraph = Graph Int

instance Arbitrary a => Arbitrary (Graph a) where
    arbitrary = sized foo

foo :: Arbitrary a => Int -> Gen (Graph a)
foo 0 = return empty
foo n | n>0 = do
    i <- arbitrary
    operator <- elements [overlay,connect]
    sub <- foo (n-1)
    return $ operator (Vertex i) sub

-- length 30
testGen = generate arbitrary :: IO JonGraph

fnargh :: Int -> IO ()
fnargh 0 = return ()
fnargh n = do
    g <- generate (foo 5) :: IO JonGraph
    writeFile ((show n)++".dot") (exportViaShow g)
    fnargh (n - 1)

main = fnargh 10

