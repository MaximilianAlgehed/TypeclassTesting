{-# LANGUAGE DeriveDataTypeable #-}
import Test.QuickCheck
import MonoidAlgebra

-- Test left identity law for Maybe
prop_left_unit :: Maybe MonoidalAlgebra -> Bool
prop_left_unit m = mappend mempty m == m

-- Test right identity law for Maybe
prop_right_unit :: Maybe MonoidalAlgebra -> Bool
prop_right_unit m = mappend m mempty == m 

-- Test associativity law for Maybe
prop_assoc :: Maybe MonoidalAlgebra -> Maybe MonoidalAlgebra -> Maybe MonoidalAlgebra -> Bool
prop_assoc x y z = mappend x (mappend y z) == mappend (mappend x y) z
