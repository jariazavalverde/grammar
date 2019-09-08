import Data.Symbol
import Data.Production
import Data.Grammar
import Data.ShortGrammar


-- | Example 1: regular grammar
--   <A> → "a" <A> | "b" <B>
--   <B> → "b" <B> | ɛ
g1 :: Grammar
g1 = Grammar
    [Terminal "a", Terminal "b"]
    [NonTerminal "A", NonTerminal "B"]
    [
        Production [NonTerminal "A"] [Terminal "a", NonTerminal "A"],
        Production [NonTerminal "A"] [Terminal "b", NonTerminal "B"],
        Production [NonTerminal "B"] [Terminal "b", NonTerminal "B"],
        Production [NonTerminal "B"] []
    ]
    (NonTerminal "A")