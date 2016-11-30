{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

-- this module declare the `Card` type and related types
module Card where

import Control.Lens
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Aeson
import GHC.Generics
import Control.DeepSeq
import qualified Data.Map as M
import Data.Monoid ((<>))

(=:) = M.singleton

data Mechanic = Mechanic T.Text
              deriving (Show, Ord, Eq, Generic)

text2Mechanic :: T.Text -> Mechanic
text2Mechanic = Mechanic

data CardSet = Classic | Basic
        | GoblinsVsGnomes | TheGrandTournament
        | WhispersOfTheOldGods | MeanStreetsOfGadgetzan
        | CurseOfNaxxramas | BlackrockMountain | TheLeagueOfExplorers
        | OneNightInKarazhan
        -- These are special set
        | Credits | TavernBrawl | Cheat | Tutorial | Promo | Unknown | Reward
    deriving (Show, Eq, Read, Generic)

cardsetMap :: M.Map String CardSet
cardsetMap =
        "Classic" =: Classic
    <>  "Basic" =: Basic
    <>  "Goblins vs Gnomes" =: GoblinsVsGnomes
    <>  "The Grand Tournament" =: TheGrandTournament
    <>  "Whispers of the Old Gods" =: WhispersOfTheOldGods
    <>  "Mean Streets Of Gadgetzan" =: MeanStreetsOfGadgetzan
    <>  "Naxxramas" =: CurseOfNaxxramas
    <>  "Blackrock Mountain" =: BlackrockMountain
    <>  "The League of Explorers" =: TheLeagueOfExplorers
    <>  "One Night in Karazhan" =: OneNightInKarazhan
    <>  "CREDITS" =: Credits
    <>  "Tavern Brawl" =: TavernBrawl
    <>  "Cheat" =: Cheat
    <>  "Tutorial" =: Tutorial
    <>  "Promo" =: Promo
    <>  "Unknown" =: Unknown
    <>  "Reward" =: Reward

text2CardSet :: T.Text -> CardSet
text2CardSet t = cardsetMap M.! T.unpack t

-- to avoid name collision with `Mechanic`, `CT` is prepended here
data CardType =   CTHeroPower | CTMinion | CTSpell | CTSecret | CTWeapon | CTHero
                | CTEnchantment
    deriving (Show, Eq, Read, Generic)

cardtypeMap :: M.Map String CardType
cardtypeMap =
        "Minion" =: CTMinion
    <>  "Spell" =: CTSpell
    <>  "Secret" =: CTSecret
    <>  "Weapon" =: CTWeapon
    <>  "Hero Power" =: CTHeroPower
    <>  "Hero" =: CTHero
    <>  "Enchantment" =: CTEnchantment

text2CardType :: T.Text -> CardType
text2CardType t = cardtypeMap M.! T.unpack t

data CardSubtype = Beast | Demon | Dragon | Murloc | Pirate | Totem | Mech | General
    deriving (Show, Eq, Read, Generic)

cardsubtypeMap :: M.Map String CardSubtype
cardsubtypeMap =
        "Beast" =: Beast
    <>  "Demon" =: Demon
    <>  "Dragon" =: Dragon
    <>  "Murloc" =: Murloc
    <>  "Pirate" =: Pirate
    <>  "Totem" =: Totem
    <>  "Mech" =: Totem
    <>  "General" =: General -- this is only needed for generating html element
    <>  "" =: General

text2CardSubtype :: T.Text -> CardSubtype
text2CardSubtype t = cardsubtypeMap M.! T.unpack t

data CardRarity = Common | Rare | Epic | Legendary | Free
    deriving (Show, Eq, Read, Generic)

cardrarityMap :: M.Map String CardRarity 
cardrarityMap =
        "Common" =: Common
    <>  "Rare" =: Rare
    <>  "Epic" =: Epic
    <>  "Legendary" =: Legendary
    <>  "Free" =: Free
    <>  "" =: Free

text2CardRarity :: T.Text -> CardRarity
text2CardRarity t = cardrarityMap M.! T.unpack t

newtype CardCost = CardCost Int deriving (Show, Generic, Eq, Ord)
newtype CardAttack = CardAttack Int deriving (Show, Generic, Eq, Ord)
newtype CardHealth = CardHealth Int deriving (Show, Generic, Eq, Ord)

text2CardCost :: T.Text -> CardCost
text2CardCost = CardCost . read . T.unpack

text2CardAttack :: T.Text -> CardAttack
text2CardAttack = CardAttack . read . T.unpack

text2CardHealth :: T.Text -> CardHealth
text2CardHealth = CardHealth . read . T.unpack

data Tag = Tag T.Text
    deriving (Show, Ord, Eq, Generic)

text2Tag :: T.Text -> Tag
text2Tag = Tag

data CardClass = Druid | Hunter | Mage | Paladin | Priest | Rogue | Shaman
               | Warlock | Warrior | Neutral
               deriving (Show, Eq, Read, Generic)

cardclassMap :: M.Map String CardClass
cardclassMap =
        "Druid" =: Druid
    <>  "Hunter" =: Hunter
    <>  "Mage" =: Mage
    <>  "Paladin" =: Paladin
    <>  "Priest" =: Priest
    <>  "Rogue" =: Rogue
    <>  "Shaman" =: Shaman
    <>  "Warlock" =: Warlock
    <>  "Warrior" =: Warrior
    <>  "Neutral" =: Neutral
    <>  "" =: Neutral

text2CardClass :: T.Text -> CardClass
text2CardClass t = cardclassMap M.! T.unpack t

data Card = Card {
    _cardName :: T.Text
  , _cardSet  :: CardSet
  , _cardType :: CardType
  , _cardSubtype :: Maybe CardSubtype
  , _cardRarity :: CardRarity
  , _cardCost   :: CardCost
  , _cardAttack :: CardAttack
  , _cardHealth :: CardHealth
  , _cardAbilities :: S.Set Mechanic
  , _cardTags :: S.Set Tag
  , _cardDesc :: T.Text
  , _cardFlavor :: T.Text
  , _cardClass :: CardClass
  } deriving (Show, Generic)
makeLenses ''Card

type Image = (T.Text, [T.Text])

instance ToJSON Card
instance ToJSON CardClass
instance ToJSON Tag
instance ToJSON CardHealth
instance ToJSON CardAttack
instance ToJSON CardCost
instance ToJSON CardRarity
instance ToJSON CardSubtype
instance ToJSON CardType
instance ToJSON CardSet
instance ToJSON Mechanic

instance FromJSON Card
instance FromJSON CardClass
instance FromJSON Tag
instance FromJSON CardHealth
instance FromJSON CardAttack
instance FromJSON CardCost
instance FromJSON CardRarity
instance FromJSON CardSubtype
instance FromJSON CardType
instance FromJSON CardSet
instance FromJSON Mechanic

instance NFData Card
instance NFData CardClass
instance NFData Tag
instance NFData CardHealth
instance NFData CardAttack
instance NFData CardCost
instance NFData CardRarity
instance NFData CardSubtype
instance NFData CardType
instance NFData CardSet
instance NFData Mechanic
