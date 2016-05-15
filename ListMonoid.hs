{-# LANGUAGE DeriveDataTypeable #-}
import Test.QuickCheck
import MonoidAlgebra
import Rewriter

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary = fmap (foldl (flip Cons) Nil) (arbitrary :: (Arbitrary a) => Gen [a])

-- Weird zipping monoid lists
instance (Monoid a) => Monoid (List a) where
    mempty = Nil

    mappend Nil m = m 
    mappend m Nil = m 
    mappend (Cons a lst) (Cons b lst2) = Cons (mappend a b) (mappend lst lst2)

-- Test left identity law for our weird list monoid
prop_left_unit :: List MonoidalAlgebra -> Bool
prop_left_unit m = mappend mempty m == m

-- Test right identity law for our weird list monoid 
prop_right_unit :: List MonoidalAlgebra -> Bool
prop_right_unit m = mappend m mempty == m 

-- Test associativity law for our weird list monoid 
prop_assoc :: List MonoidalAlgebra -> List MonoidalAlgebra -> List MonoidalAlgebra -> Bool
prop_assoc x y z = mappend x (mappend y z) == mappend (mappend x y) z
