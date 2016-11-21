{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- this module declare the `Card` type and related types
module Card where

import Control.Lens
import qualified Data.Text as T
import qualified Data.Set as S

data Mechanic = Mechanic T.Text
              deriving (Show, Ord, Eq)

data CardSet = Classic
        | GoblinsVsGnomes | TheGrandTournament
        | WhispersOfTheOld | MeanStreetsOfGadgetzan
        | CurseOfNaxxramas | BlackrockMountain | TheLeagueOfExplorers
        | OneNightInKarazhan
    deriving (Show, Eq)

-- to avoid name collision with `Mechanic`, `CT` is prepended here
data CardType = CTHeroPower | CTMinion | CTSpell | CTSecret | CTWeapon
    deriving (Show, Eq)

data CardSubtype = Beast | Demon | Dragon | Murloc | Pirate | Totem | General
    deriving (Show, Eq)

data CardRarity = Normal | Rare | Epic | Legendary | None
    deriving (Show, Eq)

newtype CardCost = CardCost Int deriving Show
newtype CardAttack = CardAttack Int deriving Show
newtype CardHealth = CardHealth Int deriving Show

data Tag = Tag T.Text
    deriving (Show, Ord, Eq)

data CardClass = Druid | Hunter | Mage | Paladin | Priest | Rogue | Shaman
               | Warlock | Warrior
               deriving (Show, Eq)

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
  , _cardClass :: Maybe CardClass
  } deriving Show
makeLenses ''Card

deathWing :: Card
deathWing = Card
    "Deathwing"
    Classic
    CTMinion
    (Just Dragon)
    Legendary
    (CardCost 10)
    (CardAttack 12)
    (CardHealth 12)
    (S.fromList
        [Mechanic "Battlecry", Mechanic "Destroy", Mechanic "Discard"])
    S.empty
    "Card Desc"
    "Card Flavor"
    Nothing

