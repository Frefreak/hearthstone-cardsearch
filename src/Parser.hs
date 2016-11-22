{-# LANGUAGE OverloadedStrings #-}

-- parse HTML from gamepedia to Card type
module Parser where

import Text.XML.Cursor
import Text.HTML.DOM
import Prelude hiding (readFile)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Monoid ((<>))
import Text.XML (Node(..))
import qualified Data.Set as S
import Network.Wreq
import Control.Lens.Operators

import Card

filterNodeContent :: T.Text -> (Node -> Bool)
filterNodeContent t (NodeContent t') = t == t'
filterNodeContent _ _ = False

-- return image url and Card type
parseCard :: Cursor -> ([T.Text], Card)
parseCard cur =
    let cardname = head $ cur $// element "div" >=> attributeIs "class" "title" &/ content
        imgUrls = cur $// element "div" >=> attributeIs "class" "image" &/
            element "a" >=> attributeIs "class" "image" &/ element "img" >=> attribute "src"
        tabCur = head $ cur $// element "table"
        cardset = text2CardSet . head $ tabCur $// element "b" &//
            checkNode (filterNodeContent "Set:") >=> parent >=> parent >=> followingSibling &/
            element "a" &/ content
        cardtype = text2CardType . T.concat $ tabCur $// element "b" &//
            checkNode (filterNodeContent "Type:") >=> parent >=> parent >=> followingSibling &/
            element "a" &/ content
        cardsubtype = if cardtype == CTMinion
                        then Just . text2CardSubtype . T.concat $ tabCur $// element "b" &//
                            checkNode (filterNodeContent "Subtype:") >=> parent >=> parent 
                            >=> followingSibling &/ element "a" &/ content
                        else Nothing
        cardrarity = text2CardRarity . T.concat $ tabCur $// element "b" &//
            checkNode (filterNodeContent "Rarity:") >=> parent >=> parent >=>
            followingSibling &/ element "a" &// content
        cardcost = text2CardCost . T.concat $ tabCur $//  element "b" &//
            checkNode (filterNodeContent "Cost:") >=> parent >=> parent >=>
            followingSibling &/ content
        cardatk = if cardtype == CTMinion || cardtype == CTWeapon
                    then text2CardAttack . T.concat $ tabCur $//  element "b" &//
                        checkNode (filterNodeContent "Attack:") >=> parent >=> parent >=>
                        followingSibling &/ content
                    else CardAttack 0
        cardhp = if cardtype == CTMinion || cardtype == CTWeapon
                    then text2CardHealth . T.concat $ tabCur $//  element "b" &//
                        checkNode (filterNodeContent "Health:") >=> parent >=> parent >=>
                        followingSibling &/ content
                    else CardHealth 0
        cardabi = S.fromList . map text2Mechanic $ tabCur $// element "b" &//
            checkNode (filterNodeContent "Abilities:") >=> parent >=> parent >=>
            followingSibling &/ element "a" &/ content
        cardtags = S.fromList . map text2Tag $ tabCur $//  element "b" &//
            checkNode (filterNodeContent "Tags:") >=> parent >=> parent >=>
            followingSibling &/ element "a" &/ content
        cardclass = text2CardClass . T.concat $ tabCur $// element "b" &//
            checkNode (filterNodeContent "Class:") >=> parent >=> parent >=>
            followingSibling &/ element "a" &/ content
        [pDesc, pFlavor, _] = cur $// element "table" >=> parent >=>
            followingSibling >=> element "p"
        cardDesc = T.concat $ pDesc $// content
        cardFlavor = T.concat $ pFlavor $// content
        card = Card cardname cardset cardtype cardsubtype cardrarity cardcost cardatk
            cardhp cardabi cardtags cardDesc cardFlavor cardclass
    in (imgUrls, card)

test :: String -> IO Card
test s = do
    r <- get $ "http://hearthstone.gamepedia.com/" ++ s
    let lbs = r ^. responseBody
        cur = fromDocument . parseLBS $ lbs
    return . snd . parseCard $ cur
