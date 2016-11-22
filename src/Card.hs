{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- this module declare the `Card` type and related types
module Card where

import Control.Lens
import qualified Data.Text as T
import qualified Data.Set as S

data Mechanic = Mechanic T.Text
              deriving (Show, Ord, Eq)

text2Mechanic :: T.Text -> Mechanic
text2Mechanic = Mechanic

data CardSet = Classic | Basic
        | GoblinsVsGnomes | TheGrandTournament
        | WhispersOfTheOldGods | MeanStreetsOfGadgetzan
        | CurseOfNaxxramas | BlackrockMountain | TheLeagueOfExplorers
        | OneNightInKarazhan
    deriving (Show, Eq)

text2CardSet :: T.Text -> CardSet
text2CardSet "Classic" = Classic
text2CardSet "Basic" = Basic
text2CardSet "Goblins vs Gnomes" = GoblinsVsGnomes
text2CardSet "The Grand Tournament" = TheGrandTournament
text2CardSet "Whispers of the Old Gods" = WhispersOfTheOldGods
text2CardSet "Mean Streets Of Gadgetzan" = MeanStreetsOfGadgetzan
text2CardSet "Curse of Naxxramas" = CurseOfNaxxramas
text2CardSet "Blackrock Mountain" = BlackrockMountain
text2CardSet "The League of Explorers" = TheLeagueOfExplorers
text2CardSet "One Night in Karazhan" = OneNightInKarazhan

-- to avoid name collision with `Mechanic`, `CT` is prepended here
data CardType = CTHeroPower | CTMinion | CTSpell | CTSecret | CTWeapon
    deriving (Show, Eq)

text2CardType :: T.Text -> CardType
text2CardType "Minion" = CTMinion
text2CardType "Spell" = CTSpell
text2CardType "Secret" = CTSecret
text2CardType "Weapon" = CTWeapon
text2CardType "Hero Power" = CTHeroPower

data CardSubtype = Beast | Demon | Dragon | Murloc | Pirate | Totem | General
    deriving (Show, Eq)

text2CardSubtype :: T.Text -> CardSubtype
text2CardSubtype "Beast" = Beast
text2CardSubtype "Demon" = Demon
text2CardSubtype "Dragon" = Dragon
text2CardSubtype "Murloc" = Murloc
text2CardSubtype "Pirate" = Pirate
text2CardSubtype "Totem" = Totem
text2CardSubtype "" = General

data CardRarity = Common | Rare | Epic | Legendary | Free
    deriving (Show, Eq)

text2CardRarity :: T.Text -> CardRarity
text2CardRarity "Common" = Common
text2CardRarity "Rare" = Rare
text2CardRarity "Epic" = Epic
text2CardRarity "Legendary" = Legendary
text2CardRarity "" = Free

newtype CardCost = CardCost Int deriving Show
newtype CardAttack = CardAttack Int deriving Show
newtype CardHealth = CardHealth Int deriving Show

text2CardCost :: T.Text -> CardCost
text2CardCost = CardCost . read . T.unpack

text2CardAttack :: T.Text -> CardAttack
text2CardAttack = CardAttack . read . T.unpack

text2CardHealth :: T.Text -> CardHealth
text2CardHealth = CardHealth . read . T.unpack

data Tag = Tag T.Text
    deriving (Show, Ord, Eq)

text2Tag :: T.Text -> Tag
text2Tag = Tag

data CardClass = Druid | Hunter | Mage | Paladin | Priest | Rogue | Shaman
               | Warlock | Warrior | Neutral
               deriving (Show, Eq)

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
  } deriving Show
makeLenses ''Card
