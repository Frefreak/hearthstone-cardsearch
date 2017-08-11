{-# LANGUAGE OverloadedStrings #-}

-- parse HTML from gamepedia to Card type
module Parser where

import Data.String (fromString)
import Text.XML.Cursor
import Text.HTML.DOM
import Prelude hiding (readFile)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Monoid ((<>))
import Text.XML (Node(..))
import qualified Data.Set as S
import Network.HTTP.Simple
import Control.Lens.Operators
import System.Timeout
import Control.Exception

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
        tabCur = head $ cur $// element "div" >=> attributeIs "class" "body" &/ element "table"
        cardset = text2CardSet . head $ tabCur $// element "b" &//
            checkNode (filterNodeContent "Set:") >=> parent >=> parent >=> followingSibling &/
            element "a" &/ content
        cardtype' = T.concat $ tabCur $// element "b" &//
            checkNode (filterNodeContent "Type:") >=> parent >=> parent >=> followingSibling &/
            element "a" &/ content
        cardtype = if cardtype' == ""
                     then text2CardType . T.strip . T.concat $ tabCur $// element "b" &//
                        checkNode (filterNodeContent "Type:") >=> parent >=> parent >=>
                        followingSibling >=> element "td" &/ content
                     else text2CardType cardtype'
        cardsubtype = if cardtype == CTMinion
                        then Just . text2CardSubtype . T.concat $ tabCur $// element "b" &//
                            checkNode (filterNodeContent "Subtype:") >=> parent >=> parent 
                            >=> followingSibling &/ element "a" &/ content
                        else Nothing
        cardrarity = text2CardRarity . T.concat $ tabCur $// element "b" &//
            checkNode (filterNodeContent "Rarity:") >=> parent >=> parent >=>
            followingSibling &/ element "a" &// content
        cardcost = if cardtype /= CTHero
                     then text2CardCost . T.concat $ tabCur $// element "b" &//
                        checkNode (filterNodeContent "Cost:") >=> parent >=> parent >=>
                        followingSibling &/ content
                     else CardCost 0
        cardatk = if cardtype == CTMinion || cardtype == CTWeapon
                    then text2CardAttack . T.concat $ tabCur $//  element "b" &//
                        checkNode (filterNodeContent "Attack:") >=> parent >=> parent >=>
                        followingSibling &/ content
                    else CardAttack 0
        cardhp = case cardtype of
                    CTMinion -> text2CardHealth . T.concat $ tabCur $// 
                        element "b" &// checkNode (filterNodeContent "Health:")
                        >=> parent >=> parent >=> followingSibling &/ content
                    CTWeapon -> text2CardHealth . T.concat $ tabCur $// 
                        element "b" &// checkNode (filterNodeContent "Durability:")
                        >=> parent >=> parent >=> followingSibling &/ content
                    _ -> CardHealth 0
        cardabi = S.fromList . map text2Mechanic $ tabCur $// element "b" &//
            checkNode (filterNodeContent "Abilities:") >=> parent >=> parent >=>
            followingSibling &/ element "a" &/ content
        cardtags = S.fromList . map text2Tag $ tabCur $//  element "b" &//
            checkNode (filterNodeContent "Tags:") >=> parent >=> parent >=>
            followingSibling &/ element "a" &/ content
        cardclass = text2CardClass . T.concat $ tabCur $// element "b" &//
            checkNode (filterNodeContent "Class:") >=> parent >=> parent >=>
            followingSibling &/ element "a" &/ content
        cardtext = cur $// element "table" >=> parent >=> followingSibling >=> element "p"
        cardDesc = T.concat $ head cardtext $// content
        cardFlavor = if cardtype /= CTHero
                       then if length cardtext <= 2
                              then ""
                              else T.concat $ (cardtext !! 1) $// content
                       else ""
        card = Card cardname cardset cardtype cardsubtype cardrarity cardcost cardatk
            cardhp cardabi cardtags cardDesc cardFlavor cardclass
    in (imgUrls, card)

hearthstoneWikiUrl :: String
hearthstoneWikiUrl = "https://hearthstone.gamepedia.com/"

mkWikiUrl :: T.Text -> String
mkWikiUrl = (hearthstoneWikiUrl ++) . T.unpack

recursiveGet :: IO a -> IO a
recursiveGet act = do
    r <- timeout 3000000 act
    case r of
        Just r' -> return r'
        Nothing -> recursiveGet act

parseLBSCard :: T.Text -> IO ([T.Text], Card)
parseLBSCard s = do
    r <- recursiveGet (httpLbs . fromString $ mkWikiUrl s)
    let lbs = getResponseBody r
        cur = fromDocument . parseLBS $ lbs
    return $ parseCard cur
