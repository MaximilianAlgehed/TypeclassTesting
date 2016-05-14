{-# LANGUAGE DeriveDataTypeable #-}
import Data.Ord
import Data.List
import Test.QuickCheck
import Prelude
import Data.Data
import Data.Generics.Uniplate.Data

data AbelianAlgebra = Var Int | Zero | Negate AbelianAlgebra | AbelianAlgebra :+: AbelianAlgebra deriving (Show, Data, Ord)

-- laws for abelian groups
rewrite_rules :: AbelianAlgebra -> Maybe AbelianAlgebra 
rewrite_rules x = toMby $ filter isJ (map ($x) rules)
    where
        isJ (Just _) = True
        isJ _        = False

        toMby [] = Nothing 
        toMby (x:_) = x

rules = [neg_zero, assoc, right_id, left_id, right_inverse, left_inverse, commutativity]

-- Negate zero = zero
neg_zero (Negate Zero) = Just Zero
neg_zero _ = Nothing

-- Associativity
assoc ((x :+: y) :+: z) = Just $ x :+: (y :+: z)
assoc _ = Nothing

-- Right identity
right_id (x :+: Zero) = Just x
right_id _ = Nothing

-- Left identity
left_id (Zero :+: x) = Just x
left_id _ = Nothing

-- inverses
right_inverse (x :+: (Negate y))
    | y == x    = Just Zero
    | otherwise = Nothing
right_inverse _  = Nothing

-- left inverse, obviously
left_inverse ((Negate y) :+: x)
    | y == x    = Just Zero 
    | otherwise = Nothing
left_inverse _ = Nothing

-- commutativity
commutativity (x :+: y) = commutativeRewrite (x :+: y)
commutativity _ = Nothing

-- Rewrite an associative commutative expression
commutativeRewrite x = if areEqual x output then Nothing else Just output  
    where
        output = unFlatten $ sort $ map simplify $ flatten x

        unFlatten [x] = x
        unFlatten (x:xs) = x :+: (unFlatten xs)

        flatten (x :+: y) = flatten x ++ flatten y
        flatten z         = [z]

-- Rewrite until we hit a ground instance
simplify = rewrite rewrite_rules

-- Equality
instance Eq AbelianAlgebra where
    m == m' = areEqual (simplify m) (simplify m') -- Use syntactic equality

-- Syntactic equality
areEqual :: AbelianAlgebra -> AbelianAlgebra -> Bool
areEqual (Var x) (Var y) = x == y
areEqual Zero Zero = True
areEqual (a :+: b) (c :+: d) = (areEqual a c) && (areEqual b d)
areEqual (Negate x) (Negate y) = areEqual x y
areEqual _ _ = False

prop_commutative :: AbelianAlgebra -> AbelianAlgebra -> Bool
prop_commutative x y = (x :+: y) == (y :+: x)

-- Arbitrary term in the algebra of Abelian Groups
instance Arbitrary AbelianAlgebra where
    arbitrary = sized arb
        where
            arb 0 = oneof [return Zero, oneof (map (return . Var) [0..10])]
            arb n = frequency [(1, return Zero), (1, oneof (map (return . Var) [0..10])), 
                                (2, do
                                    x <- arb (n `div` 2)
                                    y <- arb (n `div` 2)
                                    return (x :+: y)),
                                (2, do
                                    x <- arb (n - 1)
                                    return (Negate x))]

