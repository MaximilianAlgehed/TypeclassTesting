{-# LANGUAGE DeriveDataTypeable #-}
module MonoidAlgebra where
import Test.QuickCheck
import Data.Data
import Data.Generics.Uniplate.Data
import Rewriter

-- The initial algebra of monoids
data MonoidalAlgebra = Var Int | Mempty | Mappend MonoidalAlgebra MonoidalAlgebra deriving (Data, Typeable, Show)

-- Simple enough instance declarations
instance Monoid MonoidalAlgebra where
    mempty  = Mempty
    mappend = Mappend

rules = [left_identity, right_identity, assoc]

-- Monoid laws
-- Left identity
left_identity (Mappend Mempty m) = Just m
left_identity _ = Nothing
-- Right identity
right_identity (Mappend m Mempty) = Just m
right_identity _ = Nothing
-- Associativity
assoc (Mappend (Mappend x y) z) = Just $ Mappend x (Mappend y z)
assoc _ = Nothing

-- Rewrite until we hit a ground instance
simplify = perform_rewrite rules 

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

