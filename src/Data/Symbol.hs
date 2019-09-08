module Data.Symbol (
    Symbol(..),
    showSymbols,
    readSymbols,
    isTerminalSymbol,
    isNonTerminalSymbol
) where


import Data.List(intersperse)
import Text.Read(readMaybe)

data Symbol = Empty | Terminal String | NonTerminal String deriving (Eq, Ord)


instance Show Symbol where
    show Empty = "ɛ"
    show (Terminal x) = show x
    show (NonTerminal x) = '<' : x ++ ">"


instance Read Symbol where
    readsPrec _ ('ɛ':rest) = [(Empty, rest)]
    readsPrec _ ('"':input) = let (symbol, rest) = span (/= '"') input in [(Terminal symbol, tail rest)]
    readsPrec _ ('<':input) = let (symbol, rest) = span (/= '>') input in [(NonTerminal symbol, tail rest)]
    readsPrec _ _ = []


-- | Conversion of symbols to readable Strings.
showSymbols :: [Symbol] -> String
showSymbols [] = "ɛ"
showSymbols s = concat (intersperse " " (map show s))

-- | Conversion of symbols from readable String.
readSymbols :: String -> ([Symbol], String)
readSymbols [] = ([], "")
readSymbols (' ':xs) = readSymbols xs
readSymbols ('\t':xs) = readSymbols xs
readSymbols input = let xs = reads input
                        [(symbol, rest)] = xs
                        (symbols, rest') = readSymbols rest in
                    if null xs then ([], input)
                    else (symbol:symbols, rest')

-- | Test whether the symbol is terminal.
isTerminalSymbol :: Symbol -> Bool
isTerminalSymbol (Terminal _) = True
isTerminalSymbol _ = False

-- | Test whether the symbol is non-terminal.
isNonTerminalSymbol :: Symbol -> Bool
isNonTerminalSymbol = not.isTerminalSymbol