{-# LANGUAGE DeriveDataTypeable #-}
import Test.QuickCheck
import Prelude
import Data.Data
import Data.Generics.Uniplate.Data

-- The initial algebra of monoids
data MonoidalAlgebra = Var Int | Mempty | Mappend MonoidalAlgebra MonoidalAlgebra deriving (Data, Typeable, Show)

-- Simple enough instance declarations
instance Monoid MonoidalAlgebra where
    mempty = Mempty
    mappend = Mappend

-- Monoid laws
rewrite_rules :: MonoidalAlgebra -> Maybe MonoidalAlgebra
-- Left identity
rewrite_rules (Mappend Mempty m) = Just m
-- Right identity
rewrite_rules (Mappend m Mempty) = Just m
-- Associativity
rewrite_rules (Mappend (Mappend x y) z) = Just $ Mappend x (Mappend y z)
rewrite_rules _ = Nothing

-- Rewrite until we hit a ground instance
simplify = rewrite rewrite_rules

-- Equality
instance Eq MonoidalAlgebra where
    m == m' = areEqual (simplify m) (simplify m') -- Use syntactic equality

-- Syntactic equality
areEqual :: MonoidalAlgebra -> MonoidalAlgebra -> Bool
areEqual (Var x) (Var y) = x == y
areEqual Mempty Mempty = True
areEqual (Mappend a b) (Mappend c d) = (areEqual a c) && (areEqual b d)
areEqual _ _ = False

-- Arbitrary term in the algebra of monoids
instance Arbitrary MonoidalAlgebra where
    arbitrary = sized arb
        where
            arb 0 = oneof [return Mempty, oneof (map (return . Var) [0..10])]
            arb n = frequency [(1, return Mempty), (1, oneof (map (return . Var) [0..10])), 
                                (2, do
                                    x <- arb (n `div` 2)
                                    y <- arb (n `div` 2)
                                    return (Mappend x y))]

-- Test left identity law for Maybe
prop_left_unit :: Maybe MonoidalAlgebra -> Bool
prop_left_unit m = mappend mempty m == m

-- Test right identity law for Maybe
prop_right_unit :: Maybe MonoidalAlgebra -> Bool
prop_right_unit m = mappend m mempty == m 

-- Test associativity law for Maybe
prop_assoc :: Maybe MonoidalAlgebra -> Maybe MonoidalAlgebra -> Maybe MonoidalAlgebra -> Bool
prop_assoc x y z = mappend x (mappend y z) == mappend (mappend x y) z
