module Data.ShortGrammar(
    ShortGrammar(..)
) where


import Data.Symbol(showSymbols)
import Data.Production(Production(..))
import Data.Grammar(Grammar(..), getProductionsByLeftSymbols)
import Data.List(nub, intersperse, isInfixOf)


newtype ShortGrammar = ShortGrammar {
    getShortGrammar :: Grammar
} deriving (Eq, Ord)


instance Show ShortGrammar where
    show (ShortGrammar g) = (concat.(intersperse "\n").(map f).nub.(map getLeftSymbols).getProductions) g
        where f s = showSymbols s
                    ++ " → "
                    ++ concat (intersperse " | " (map (showSymbols.getRightSymbols) (getProductionsByLeftSymbols g s)))