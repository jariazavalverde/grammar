module Data.Grammar(
    Grammar(..),
    isContextFreeGrammar,
    isRegularGrammar,
    getProductionsByLeftSymbols,
    removeDirectLeftRecursion,
    removeLeftRecursion
) where


import Data.Symbol(Symbol(..))
import Data.Production(Production(..), isDirectlyLeftRecursive)
import Data.List(intersperse, isInfixOf)


data Grammar = Grammar {
    getTerminalSymbols :: [Symbol],
    getNonTerminalSymbols :: [Symbol],
    getProductions :: [Production],
    getStartSymbol :: Symbol
} deriving Ord


instance Show Grammar where
    show x = concat $ intersperse "\n" (map show (getProductions x))


instance Eq Grammar where
    x == y = isInfixOf (getProductions x) (getProductions y)


-- | Test whether the grammar is cotext-free.
isContextFreeGrammar :: Grammar -> Bool
isContextFreeGrammar = (all contextFree).(map getLeftSymbols).getProductions
    where contextFree [NonTerminal _] = True
          contextFree _ = False

-- | Test whether the grammar is regular.
isRegularGrammar :: Grammar -> Bool
isRegularGrammar g = and $ map ($ g) [isContextFreeGrammar, (all regular).(map getRightSymbols).getProductions]
    where regular [] = True
          regular [Terminal _] = True
          regular [Terminal _, NonTerminal _] = True
          regular _ = False

-- | Find the productions of the grammar by left symbols.
getProductionsByLeftSymbols :: Grammar -> [Symbol] -> [Production]
getProductionsByLeftSymbols g s = filter ((== s).getLeftSymbols) (getProductions g)

-- | Remove direct left-recursion of the grammar.
removeDirectLeftRecursion :: Grammar -> Grammar
removeDirectLeftRecursion = error "not implemented yet"

-- | Remove all left-recursion of the grammar.
removeLeftRecursion :: Grammar -> Grammar
removeLeftRecursion = error "not implemented yet"