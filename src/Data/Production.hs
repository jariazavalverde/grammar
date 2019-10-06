{-# LANGUAGE TemplateHaskell #-}
module Data.Production (
    Production(..),
    readProductions,
    isDirectlyLeftRecursive
) where


import Data.Symbol(Symbol(..), showSymbols, readSymbols)
import Language.Haskell.TH(conE, appE)
import Language.Haskell.TH.Syntax(Lift(..))


data Production = Production {
    getLeftSymbols :: [Symbol],
    getRightSymbols :: [Symbol]
} deriving (Eq, Ord)


instance Show Production where
    show (Production x y) = showSymbols x ++ " → " ++ showSymbols y


instance Read Production where
    readsPrec _ input = let (left, rest) = readSymbols input
                            rest' = dropWhile (== ' ') rest
                            (right, rest'') = readSymbols (tail rest')
                            (right', rest''') = readSymbols (drop 2 rest')
                            checkRight xs = length xs == 1 || length xs > 1 && not (Empty `elem` xs)
                            transformRight [Empty] = []
                            transformRight xs = xs in
                        if null left || Empty `elem` left then [] else 
                            if head rest' == '→' && checkRight right then [(Production left (transformRight right), rest'')]
                            else if take 2 rest' == "->" && checkRight right' then [(Production left (transformRight right'), rest''')]
                        else []


instance Lift Production where
    lift (Production ls rs) = appE (appE (conE 'Production) (lift ls)) (lift rs)                      


-- | Conversion of productions from readable String.
readProductions :: String -> ([Production], String)
readProductions [] = ([], "")
readProductions (' ':xs) = readProductions xs
readProductions ('\t':xs) = readProductions xs
readProductions ('\n':xs) = readProductions xs
readProductions input = let xs = reads input
                            [(production, rest)] = xs
                            (productions, rest') = readProductions rest
                        in
                        if null xs then ([], input)
                        else (production:productions, rest')

-- | Test whether the production is directly left-recursive.
isDirectlyLeftRecursive :: Production -> Bool
isDirectlyLeftRecursive (Production (x:_) (y:_)) = x == y
isDirectlyLeftRecursive _ = False