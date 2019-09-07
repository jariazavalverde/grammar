import Data.List(intersperse, isInfixOf)

data Symbol = Terminal String | NonTerminal String deriving (Eq, Ord)

data Production = Production {
    getLeftSymbols :: [Symbol],
    getRightSymbols :: [Symbol]
} deriving (Eq, Ord)

data Grammar = Grammar {
    getTerminalSymbols :: [Symbol],
    getNonTerminalSymbols :: [Symbol],
    getProductions :: [Production],
    getStartSymbol :: Symbol
} deriving Ord

instance Show Symbol where
    show (Terminal x) = x
    show (NonTerminal x) = '<' : x ++ ">"

instance Show Production where
    show (Production x []) = concat (map show x) ++ " → " ++ "ɛ"
    show (Production x y) = concat (map show x) ++ " → " ++ concat (map show y)

instance Show Grammar where
    show x = concat $ intersperse "\n" (map show (getProductions x))

instance Eq Grammar where
    x == y = isInfixOf (getProductions x) (getProductions y)

isTerminalSymbol :: Symbol -> Bool
isTerminalSymbol (Terminal _) = True
isTerminalSymbol _ = False

isNonTerminalSymbol :: Symbol -> Bool
isNonTerminalSymbol = not.isTerminalSymbol

isContextFreeGrammar :: Grammar -> Bool
isContextFreeGrammar = (all contextFree).(map getLeftSymbols).getProductions
    where contextFree [NonTerminal _] = True
          contextFree _ = False

isRegularGrammar :: Grammar -> Bool
isRegularGrammar g = and $ map ($ g) [isContextFreeGrammar, (all regular).(map getRightSymbols).getProductions]
    where regular [] = True
          regular [Terminal _] = True
          regular [Terminal _, NonTerminal _] = True
          regular _ = False

g1 :: Grammar
g1 = Grammar
    [Terminal "a"]
    [NonTerminal "A"]
    [
        Production [NonTerminal "A"] [Terminal "a", NonTerminal "A"],
        Production [NonTerminal "A"] []
    ]
    (NonTerminal "A")
