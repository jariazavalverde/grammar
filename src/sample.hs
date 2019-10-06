{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
import Data.Symbol
import Data.Production
import Data.Grammar
import Data.ShortGrammar
import Language.Haskell.TH.Syntax(Lift(..))


-- | Example 1: regular grammar
--   <A> → "a" <A> | "b" <B>
--   <B> → "b" <B> | ɛ
g1 :: Grammar
g1 = Grammar
    [NonTerminal "A", NonTerminal "B"]
    [Terminal "a", Terminal "b"]
    [
        Production [NonTerminal "A"] [Terminal "a", NonTerminal "A"],
        Production [NonTerminal "A"] [Terminal "b", NonTerminal "B"],
        Production [NonTerminal "B"] [Terminal "b", NonTerminal "B"],
        Production [NonTerminal "B"] []
    ]
    (NonTerminal "A")


-- | Example 2: directly left-recursive grammar
--   <A> → <A> "a1" <B> | <A> "a2" | "a3" | "a4"
--   <B> → <B> "b1" <C> | <B> "b2" | "b3" | "b4"
--   <C> → "c" <C> | ɛ
g2 :: Grammar
g2 = Grammar
    [NonTerminal "A", NonTerminal "B", NonTerminal "C"]
    [Terminal "a1", Terminal "a2", Terminal "a3", Terminal "a4"]
    [
        Production [NonTerminal "A"] [NonTerminal "A", Terminal "a1", NonTerminal "B"],
        Production [NonTerminal "A"] [NonTerminal "A", Terminal "a2"],
        Production [NonTerminal "A"] [Terminal "a3"],
        Production [NonTerminal "A"] [Terminal "a4"],
        Production [NonTerminal "B"] [NonTerminal "B", Terminal "b1", NonTerminal "C"],
        Production [NonTerminal "B"] [NonTerminal "B", Terminal "b2"],
        Production [NonTerminal "B"] [Terminal "b3"],
        Production [NonTerminal "B"] [Terminal "b4"],
        Production [NonTerminal "C"] [Terminal "c", NonTerminal "C"],
        Production [NonTerminal "C"] []
    ]
    (NonTerminal "A")


-- | Example 3: example 1 with TH
g3 :: Grammar
g3 = [grammar|
    <A> → "a" <A>
    <A> → "b" <B>
    <B> → "b" <B>
    <B> → ɛ
|]