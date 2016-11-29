{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Widget where

import Reflex.Dom hiding (select)
import Data.Default
import Control.Monad
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import qualified Data.Map as M
import JavaScript.JQuery (select, JQuery(..))
import qualified Data.JSString as S
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as U
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map as M
import qualified Data.Set as S
import Safe

import Card hiding ((=:))
import Filter


performArg :: MonadWidget t m => (b -> IO a) -> Event t b -> m (Event t a)
performArg f x = performEvent (fmap (liftIO . f) x)

printOnChange :: (MonadWidget t m, Show a) => String -> Dynamic t a -> m ()
printOnChange prefix dyn = do
    performArg (\s -> putStrLn (prefix ++ ": " ++ show s)) (updated dyn)
    return ()

topWidget :: MonadWidget t m => m ()
topWidget =
    divClass "container" $
        divClass "row" $
            elAttr "form" ("class" =: "col s9") $ do
                nw <- nameWidget
                dw <- descWidget
                fw <- flavorWidget
                sw <- cardsetWidget
                stw <- cardtypeWidget
                rw <- cardrarityWidget
                cw <- cardclassWidget
                suw <- cardsubtypeWidget
                ccw <- cardcostWidget
                caw <- cardatkWidget
                chw <- cardhpWidget

                cards <- liftIO getCardData

                let abis = cards ^.. traverse . cardAbilities
                    tags = cards ^.. traverse . cardTags

                aw <- abiWidget . S.toList $ S.unions abis
                tw <- tagWidget . S.toList $ S.unions tags

                -- debug VVVVVV
                printOnChange "Card Name" nw
                printOnChange "Card Description" dw
                printOnChange "Card Flavor" fw
                printOnChange "Card Set" sw
                -- debug ^^^^^^

                ev <- getPostBuild
                performArg (const $ material_select >> initialize_multiple_select) ev

                nF <- mapDyn nameRule nw
                dF <- mapDyn descRule dw
                fF <- mapDyn flavorRule fw
                sF <- mapDyn cardsetRule sw
                tF <- combineDyn cardtypeRule stw suw
                rF <- mapDyn cardrarityRule rw
                cF <- mapDyn cardclassRule cw
                ccF <- mapDyn cardcostRule ccw
                caF <- mapDyn cardatkRule caw
                chF <- mapDyn cardhpRule chw
                aF <- mapDyn abiRule aw
                tF <- mapDyn tagRule tw

                allFilter <- sequenceDyn
                    [nF, dF, fF, sF, tF, rF, cF, ccF, caF, chF, aF, tF]
                cards' <- mapDyn (\f -> applyAllFilter (concat f) cards) allFilter

                let helper c = if length c > 5
                                 then "too many cards"
                                 else T.intercalate " | " $ c ^.. traverse . cardName
                printOnChange "Avail" =<< mapDyn helper cards'
                return ()

nameRule :: String -> [Filter]
nameRule s = [nameFilter $ T.pack s]

descRule :: String -> [Filter]
descRule s = [descFilter $ T.pack s]

flavorRule :: String -> [Filter]
flavorRule s = [flavorFilter $ T.pack s]

cardsetRule :: [CardSet] -> [Filter]
cardsetRule cs = [cardsetFilter cs]

cardtypeRule :: [CardType] -> [CardSubtype] -> [Filter]
cardtypeRule ts ss = [typeFilter (CTMinion : ts), subtypeFilter ss]

cardrarityRule :: [CardRarity] -> [Filter]
cardrarityRule rs = [rarityFilter rs]

cardclassRule :: [CardClass] -> [Filter]
cardclassRule cs = [classFilter cs]

cardcostRule :: (Int, Int) -> [Filter]
cardcostRule c = [costFilter c]

cardatkRule :: (Int, Int) -> [Filter]
cardatkRule c = [atkFilter c]

cardhpRule :: (Int, Int) -> [Filter]
cardhpRule c = [hpFilter c]

abiRule :: [Mechanic] -> [Filter]
abiRule m = [abiFilter m]

tagRule :: [Tag] -> [Filter]
tagRule t = [tagFilter t]

sequenceDyn :: MonadWidget t m => [Dynamic t a] -> m (Dynamic t [a])
sequenceDyn (a:as) = do
    ras <- sequenceDyn as
    combineDyn (:) a ras
sequenceDyn [] = return $ constDyn []

nameWidget :: MonadWidget t m => m (Dynamic t String)
nameWidget =
    divClass "input-field col s4" $ do
        ti <- textInput $ def & textInputConfig_attributes .~ constDyn ("id" =: "cardname")
        elAttr "label" ("for" =: "cardname") (text "Card Name")
        return $ ti ^. textInput_value

descWidget :: MonadWidget t m => m (Dynamic t String)
descWidget =
    divClass "input-field col s4" $ do
        ti <- textInput $ def & textInputConfig_attributes .~ constDyn ("id" =: "carddesc")
        elAttr "label" ("for" =: "carddesc") (text "Card Description")
        return $ ti ^. textInput_value

flavorWidget :: MonadWidget t m => m (Dynamic t String)
flavorWidget =
    divClass "input-field col s4" $ do
        ti <- textInput $ def & textInputConfig_attributes .~ constDyn ("id" =: "cardflavor")
        elAttr "label" ("for" =: "cardflavor") (text "Card Flavor Text")
        return $ ti ^. textInput_value

cardsetWidget :: MonadWidget t m => m (Dynamic t [CardSet])
cardsetWidget = do
    dyn <- divClass "input-field col s7" $ 
        multipleSelectWidget "cardset" "Card Set" $ fmap show cardsetMap
    mapDyn (map read . words) dyn

cardtypeWidget :: MonadWidget t m => m (Dynamic t [CardType])
cardtypeWidget = do
    dyn <- divClass "input-field col s4 offset-s1" $
        multipleSelectWidget "cardtype" "Card Type" $ fmap show cardtypeMap
    mapDyn (map read . words) dyn

cardrarityWidget :: MonadWidget t m => m (Dynamic t [CardRarity])
cardrarityWidget = do
    dyn <- divClass "input-field col s3" $
        multipleSelectWidget "cardrarity" "Card Rarity" $ fmap show cardrarityMap
    mapDyn (map read . words) dyn

cardclassWidget :: MonadWidget t m => m (Dynamic t [CardClass])
cardclassWidget = do
    dyn <- divClass "input-field col s3 offset-s1" $
        multipleSelectWidget "cardclass" "Card Class" $ fmap show cardclassMap
    mapDyn (map read . words) dyn

cardsubtypeWidget :: MonadWidget t m => m (Dynamic t [CardSubtype])
cardsubtypeWidget = do
    dyn <- divClass "input-field col s4 offset-s1" $
        multipleSelectWidget "cardsubtype" "Card Subtype (Assume Minion)"
            $ fmap show cardsubtypeMap
    mapDyn (map read . words) dyn

cardcostWidget :: MonadWidget t m => m (Dynamic t (Int, Int))
cardcostWidget =
    elAttr "div" ("class" =: "input-field col s4" <> "id" =: "cost")
        $ rangeWidget "cost"

cardatkWidget :: MonadWidget t m => m (Dynamic t (Int, Int))
cardatkWidget =
    elAttr "div" ("class" =: "input-field col s4" <> "id" =: "attack")
        $ rangeWidget "attack"

cardhpWidget :: MonadWidget t m => m (Dynamic t (Int, Int))
cardhpWidget =
    elAttr "div" ("class" =: "input-field col s4" <> "id" =: "health")
        $ rangeWidget "health"

abiWidget :: MonadWidget t m => [Mechanic] -> m (Dynamic t [Mechanic])
abiWidget ms = do
    w <- checkboxWidget "Abilities" $ map (\(Mechanic s) -> T.unpack s) ms
    mapDyn (map $ Mechanic . T.pack) w

tagWidget :: MonadWidget t m => [Tag] -> m (Dynamic t [Tag])
tagWidget ms = do
    w <- checkboxWidget "Tags" $ map (\(Tag s) -> T.unpack s) ms
    mapDyn (map $ Tag . T.pack) w

nullAttr :: String -> M.Map String String
nullAttr s = s =: ""

nullAttrs :: [String] -> M.Map String String
nullAttrs = foldr (\a b -> nullAttr a <> b) (M.fromList [])

-- a commonly used widget (with materialize css framework)
-- Map is of type `Map (shown text) (value text)`
multipleSelectWidget :: MonadWidget t m => String -> String -> M.Map String String
    -> m (Dynamic t String)
multipleSelectWidget wid l opts = do
    elAttr "select" ("multiple" =: "" <> "id" =: wid) $ do
        elAttr "option" (nullAttrs ["value", "disabled", "selected"]) (text "None")
        forM (M.keys opts) $ \k -> unless (null k) $
            elAttr "option" ("value" =: (opts M.! k)) (text k)
    el "label" $ text l
    (el, _) <- elAttr' "div" ("id" =: (wid ++ "-ob") <> "style" =: "display:none;")
        $ text "observer to track the value of multi-select using click event"
    let ev = domEvent Click el
    ev' <- performArg (const . getValue $ "#" ++ wid) ev
    holdDyn "" ev'

-- a special two input widget of type `number`
rangeWidget :: MonadWidget t m => String -> m (Dynamic t (Int, Int))
rangeWidget n = do
    mi <- divClass "col s5" $ do
        elAttr "label" ("class" =: "col s6" <> "for" =: ("min" ++ n)) (text $ "min " ++ n)
        ti <- textInput $ def & textInputConfig_inputType .~ "number"
                        & textInputConfig_attributes .~ constDyn ("id" =: ("min" ++ n))
        return $ ti ^. textInput_value
    ma <- divClass "col s5 offset-s2" $ do
        elAttr "label" ("class" =: "col s6") (text $ "max " ++ n)
        ti <- textInput $ def & textInputConfig_inputType .~ "number"
                        & textInputConfig_attributes .~ constDyn (nullAttr "value")
        return $ ti ^. textInput_value
    combineDyn (\a b -> (readDef 0 a, readDef 999 b)) mi ma

-- widget used to render abilities and tags filter
checkboxWidget :: MonadWidget t m => String -> [String] -> m (Dynamic t [String])
checkboxWidget hdr strs =
    divClass "input-field col s12" $ do
        elAttr "p" ("class" =: "grey-text") $ text hdr
        dyns <- forM strs $ \s ->
            divClass "col l3 s4" $ do
                cb <- checkbox False
                    (def & checkboxConfig_attributes .~ constDyn ("id" =: s))
                elAttr "label" ("for" =: s) $ text s
                combineDyn (,) (cb ^. checkbox_value) (constDyn s)
        bs <- sequenceDyn dyns
        mapDyn (map snd . filter fst) bs

getValue :: String -> IO String
getValue s = do
    jq <- select $ S.pack s
    jss <- getListOfString jq
    return $ S.unpack jss

foreign import javascript unsafe "$1.val().join(' ')"
    getListOfString :: JQuery -> IO S.JSString
foreign import javascript unsafe "$r = cards;"
    _getCardData :: IO S.JSString
foreign import javascript unsafe "$('select').material_select();"
    material_select :: IO ()
foreign import javascript unsafe "$('#cardset').on('change', function(e) { $('#cardset-ob').trigger('click'); }); $('#cardtype').on('change', function(e) { $('#cardtype-ob').trigger('click'); }); $('#cardrarity').on('change', function(e) { $('#cardrarity-ob').trigger('click'); }); $('#cardclass').on('change', function(e) { $('#cardclass-ob').trigger('click'); }); $('#cardsubtype').on('change', function(e) { $('#cardsubtype-ob').trigger('click'); });"
    initialize_multiple_select :: IO ()

getCardData :: IO [Card]
getCardData = do
    print "loaded"
    lbs <- U.fromString . S.unpack <$> _getCardData
    let Just cards = decode lbs
    return cards

