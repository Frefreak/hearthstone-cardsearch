module Summary where

import Data.List (nub)
import qualified Data.Set as S

import Card

allMechanic :: [Card] -> [Mechanic]
allMechanic = S.toList . S.unions . map _cardAbilities

allTags :: [Card] -> [Tag]
allTags = S.toList . S.unions . map _cardTags
