module Data.Production (
    Production(..),
    isDirectlyLeftRecursive
) where


import Data.Symbol(Symbol(..), showSymbols)


data Production = Production {
    getLeftSymbols :: [Symbol],
    getRightSymbols :: [Symbol]
} deriving (Eq, Ord)


instance Show Production where
    show (Production x y) = showSymbols x ++ " → " ++ showSymbols y


-- | Test whether the production is directly left-recursive.
isDirectlyLeftRecursive :: Production -> Bool
isDirectlyLeftRecursive (Production (x:_) (y:_)) = x == y
isDirectlyLeftRecursive _ = False