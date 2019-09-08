module Data.Grammar(
    Grammar(..),
    isContextFreeGrammar,
    isRegularGrammar,
    getProductionsByLeftSymbols,
    removeDirectLeftRecursion,
    removeDirectLeftRecursionFor,
    removeLeftRecursion
) where


import Data.Symbol(Symbol(..), isTerminalSymbol, isNonTerminalSymbol)
import Data.Production(Production(..), readProductions, isDirectlyLeftRecursive)
import Data.List(nub, sort, intersperse, isInfixOf, partition)


data Grammar = Grammar {
    getNonTerminalSymbols :: [Symbol],
    getTerminalSymbols :: [Symbol],
    getProductions :: [Production],
    getStartSymbol :: Symbol
} deriving Ord


instance Show Grammar where
    show x = concat $ intersperse "\n" (map show (getProductions x))


instance Read Grammar where
    readsPrec _ input = let (p, rest) = readProductions input
                            symbols = concat (map getLeftSymbols p) ++ concat (map getRightSymbols p)
                            n = sort $ nub $ filter isNonTerminalSymbol symbols
                            e = sort $ nub $ filter isNonTerminalSymbol symbols
                            i = head $ filter isNonTerminalSymbol symbols
                        in if null p then [] else [(Grammar n e p i, rest)]


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
removeDirectLeftRecursion g = foldl removeDirectLeftRecursionFor g symbols
    where (leftrec, _) = partition isDirectlyLeftRecursive (getProductions g)
          symbols = nub $ map (head.getLeftSymbols) leftrec

-- | Remove direct left-recursion of the grammar for a non-terminal symbol.
removeDirectLeftRecursionFor :: Grammar -> Symbol -> Grammar
removeDirectLeftRecursionFor _ (Terminal _) = error "non-terminal symbol expected"
removeDirectLeftRecursionFor g@(Grammar n e p i) s = Grammar (s' : n) e (a ++ a' ++ p') i
    where (leftrec, nonrec) = span isDirectlyLeftRecursive (getProductionsByLeftSymbols g [s])
          s' = renameNonTerminalSymbol g s
          a = map ((Production [s]).(++ [s']).getRightSymbols) nonrec
          a' =  map ((Production [s']).(++ [s']).tail.getRightSymbols) leftrec ++ [Production [s'] []]
          p' = filter ((/= [s]).getLeftSymbols) p

-- | Remove all left-recursion of the grammar.
removeLeftRecursion :: Grammar -> Grammar
removeLeftRecursion = error "not implemented yet"

-- | Rename a non-terminal symbol.
renameNonTerminalSymbol :: Grammar -> Symbol -> Symbol
renameNonTerminalSymbol _ (Terminal _) = error "non-terminal symbol expected"
renameNonTerminalSymbol (Grammar n _ _ _) (NonTerminal x) = NonTerminal $ until (not.(`elem` n).NonTerminal) (++ "'") x