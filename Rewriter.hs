module Rewriter where
import Data.Data
import Data.Generics.Uniplate.Data

-- Perform a set of rewrite rules until fixpoint
perform_rewrite rules expression = rewrite (\x -> toMby $ filter isJ (map ($x) rules)) expression
    where
        isJ (Just _) = True
        isJ _        = False

        toMby [] = Nothing 
        toMby (x:_) = x
