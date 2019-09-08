module Data.Symbol (
    Symbol(..),
    showSymbols,
    isTerminalSymbol,
    isNonTerminalSymbol
) where


import Data.List(intersperse)


data Symbol = Terminal String | NonTerminal String deriving (Eq, Ord)


instance Show Symbol where
    show (Terminal x) = show x
    show (NonTerminal x) = '<' : x ++ ">"


-- | Conversion of symbols to readable Strings.
showSymbols :: [Symbol] -> String
showSymbols [] = "ɛ"
showSymbols s = concat (intersperse " " (map show s))

-- | Test whether the symbol is terminal.
isTerminalSymbol :: Symbol -> Bool
isTerminalSymbol (Terminal _) = True
isTerminalSymbol _ = False

-- | Test whether the symbol is non-terminal.
isNonTerminalSymbol :: Symbol -> Bool
isNonTerminalSymbol = not.isTerminalSymbol