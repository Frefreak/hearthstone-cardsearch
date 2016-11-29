{-# LANGUAGE OverloadedStrings #-}
module Filter where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import Data.Maybe (fromJust, isJust)
import Control.Lens
import Data.List
import qualified Data.Set as S

import Card

getAllCardData :: IO [Card]
getAllCardData = fromJust . decode <$> LBS.readFile "/home/adv_zxy/hearthstone-cardsearch/card-data.json"

data Filter = Filter ([Card] -> [Card])

nameFilter :: T.Text -> Filter
nameFilter n = Filter $
    case n of
        "" -> id
        n' -> filter (\c -> T.toLower n' `T.isInfixOf` T.toLower (c ^. cardName))

descFilter :: T.Text -> Filter
descFilter n = Filter $
    case n of
        "" -> id
        n' -> filter (\c -> T.toLower n' `T.isInfixOf` T.toLower (c ^. cardDesc))

flavorFilter :: T.Text -> Filter
flavorFilter n = Filter $
    case n of
        "" -> id
        n' -> filter (\c -> T.toLower n' `T.isInfixOf` T.toLower (c ^. cardFlavor))

cardsetFilter :: [CardSet] -> Filter
cardsetFilter s = Filter $
    if null s
      then id
      else filter (\c -> c ^. cardSet `elem` s)

typeFilter :: [CardType] -> Filter
typeFilter t = Filter $
    if null t
      then id
      else filter (\c -> c ^. cardType `elem` t)

subtypeFilter :: [CardSubtype] -> Filter
subtypeFilter st = Filter $
    if null st
      then id
      else filter (\c -> isJust (c ^. cardSubtype) &&
        fromJust (c ^. cardSubtype) `elem` st)

rarityFilter :: [CardRarity] -> Filter
rarityFilter r = Filter $
    if null r
      then id
      else filter (\c -> c ^. cardRarity `elem` r)

costFilter :: (Int, Int) -> Filter
costFilter (l, h) = Filter $
    filter (\c -> let CardCost co = c ^. cardCost
                    in co >= l && co <= h)

atkFilter :: (Int, Int) -> Filter
atkFilter (l, h) = Filter $
    filter (\c -> let CardAttack ca = c ^. cardAttack
                    in ca >= l && ca <= h)

hpFilter :: (Int, Int) -> Filter
hpFilter (l, h) = Filter $
    filter (\c -> let CardHealth ch = c ^. cardHealth
                    in ch >= l && ch <= h)

abiFilter :: [Mechanic] -> Filter
abiFilter m = Filter $
    filter (\c -> let cm = c ^. cardAbilities
                    in all (`S.member` cm) m)

tagFilter :: [Tag] -> Filter
tagFilter t = Filter $
    filter (\c -> let ct = c ^. cardTags
                    in all (`S.member` ct) t) 

classFilter :: [CardClass] -> Filter
classFilter cc = Filter $
    if null cc
      then id
      else filter (\c -> let cla = c ^. cardClass in cla `elem` cc)

applyFilter :: Filter -> [Card] -> [Card]
applyFilter (Filter f) = f

applyAllFilter :: [Filter] -> [Card] -> [Card]
applyAllFilter fs cs = foldl' (flip applyFilter) cs fs
