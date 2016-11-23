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
    deriving (Show, Eq, Generic)

text2CardSet :: T.Text -> CardSet
text2CardSet "Classic" = Classic
text2CardSet "Basic" = Basic
text2CardSet "Goblins vs Gnomes" = GoblinsVsGnomes
text2CardSet "The Grand Tournament" = TheGrandTournament
text2CardSet "Whispers of the Old Gods" = WhispersOfTheOldGods
text2CardSet "Mean Streets Of Gadgetzan" = MeanStreetsOfGadgetzan
text2CardSet "Naxxramas" = CurseOfNaxxramas
text2CardSet "Blackrock Mountain" = BlackrockMountain
text2CardSet "The League of Explorers" = TheLeagueOfExplorers
text2CardSet "One Night in Karazhan" = OneNightInKarazhan
text2CardSet "CREDITS" = Credits
text2CardSet "Tavern Brawl" = TavernBrawl
text2CardSet "Cheat" = Cheat
text2CardSet "Tutorial" = Tutorial
text2CardSet "Promo" = Promo
text2CardSet "Unknown" = Unknown
text2CardSet "Reward" = Reward


-- to avoid name collision with `Mechanic`, `CT` is prepended here
data CardType =   CTHeroPower | CTMinion | CTSpell | CTSecret | CTWeapon | CTHero
                | CTEnchantment
    deriving (Show, Eq, Generic)

text2CardType :: T.Text -> CardType
text2CardType "Minion" = CTMinion
text2CardType "Spell" = CTSpell
text2CardType "Secret" = CTSecret
text2CardType "Weapon" = CTWeapon
text2CardType "Hero Power" = CTHeroPower
text2CardType "Hero" = CTHero
text2CardType "Enchantment" = CTEnchantment


data CardSubtype = Beast | Demon | Dragon | Murloc | Pirate | Totem | Mech | General
    deriving (Show, Eq, Generic)

text2CardSubtype :: T.Text -> CardSubtype
text2CardSubtype "Beast" = Beast
text2CardSubtype "Demon" = Demon
text2CardSubtype "Dragon" = Dragon
text2CardSubtype "Murloc" = Murloc
text2CardSubtype "Pirate" = Pirate
text2CardSubtype "Totem" = Totem
text2CardSubtype "Mech" = Totem
text2CardSubtype "" = General

data CardRarity = Common | Rare | Epic | Legendary | Free
    deriving (Show, Eq, Generic)

text2CardRarity :: T.Text -> CardRarity
text2CardRarity "Common" = Common
text2CardRarity "Rare" = Rare
text2CardRarity "Epic" = Epic
text2CardRarity "Legendary" = Legendary
text2CardRarity "Free" = Free
text2CardRarity "" = Free

newtype CardCost = CardCost Int deriving (Show, Generic)
newtype CardAttack = CardAttack Int deriving (Show, Generic)
newtype CardHealth = CardHealth Int deriving (Show, Generic)

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
               deriving (Show, Eq, Generic)

text2CardClass :: T.Text -> CardClass
text2CardClass "Druid" = Druid
text2CardClass "Hunter" = Hunter
text2CardClass "Mage" = Mage
text2CardClass "Paladin" = Paladin
text2CardClass "Priest" = Priest
text2CardClass "Rogue" = Rogue
text2CardClass "Shaman" = Shaman
text2CardClass "Warlock" = Warlock
text2CardClass "Warrior" = Warrior
text2CardClass "Neutral" = Neutral
text2CardClass "" = Neutral

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

data Image = Image {
    _iname :: T.Text
  , _iurl :: [T.Text]
  } deriving (Show, Generic)

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
instance ToJSON Image

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
instance FromJSON Image

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
instance NFData Image
