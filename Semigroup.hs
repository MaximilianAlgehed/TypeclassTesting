{-# LANGUAGE DeriveDataTypeable #-}
import Test.QuickCheck
import Prelude
import Data.Data
import Data.Generics.Uniplate.Data

class Semigroup a where
    (<>) :: a -> a -> a -- Associative operator

-- The initial algebra of monoids
data SemigroupAlgebra = Var Int | SemigroupAlgebra :<>: SemigroupAlgebra deriving (Data, Typeable, Show)

-- Simple enough instance declarations
instance Semigroup SemigroupAlgebra where
    (<>) = (:<>:)

-- Semigroup laws
rewrite_rules :: SemigroupAlgebra -> Maybe SemigroupAlgebra 
-- Associativity
rewrite_rules ((x :<>: y) :<>: z) = Just $ x :<>: (y :<>: z)
rewrite_rules _ = Nothing

-- Rewrite until we hit a ground instance
simplify = rewrite rewrite_rules

-- Equality
instance Eq SemigroupAlgebra where
    m == m' = areEqual (simplify m) (simplify m') -- Use syntactic equality

-- Syntactic equality
areEqual :: SemigroupAlgebra -> SemigroupAlgebra -> Bool
areEqual (Var x) (Var y) = x == y
areEqual (a :<>: b) (c :<>: d) = (areEqual a c) && (areEqual b d)
areEqual _ _ = False

-- Arbitrary term in the algebra of monoids
instance Arbitrary SemigroupAlgebra where
    arbitrary = sized arb
        where
            arb 0 = do
                        x <- oneof $ map return [0..10]
                        return (Var x)
            arb n = frequency [(1, oneof (map (return . Var) [0..10])), 
                               (2, do
                                    x <- arb (n `div` 2)
                                    y <- arb (n `div` 2)
                                    return (x :<>: y))]

-- The type a + () (which is isomorphic to maybe...)
data PlusUnit a = A a | Unit deriving (Eq, Show)

instance (Semigroup a) => Monoid (PlusUnit a) where

    mempty = Unit

    mappend Unit m = m
    mappend m Unit = m
    mappend (A a) (A b) = A (a <> b)

instance (Arbitrary a) => Arbitrary (PlusUnit a) where
    arbitrary = oneof [return Unit, arbitrary >>= (return . A)]

-- Test left identity law for PlusUnit 
prop_left_unit :: PlusUnit SemigroupAlgebra -> Bool
prop_left_unit m = mappend mempty m == m

-- Test right identity law for PlusUnit 
prop_right_unit :: PlusUnit SemigroupAlgebra -> Bool
prop_right_unit m = mappend m mempty == m 

-- Test associativity law for PlusUnit 
prop_assoc :: PlusUnit SemigroupAlgebra -> PlusUnit SemigroupAlgebra -> PlusUnit SemigroupAlgebra -> Bool
prop_assoc x y z = mappend x (mappend y z) == mappend (mappend x y) z
