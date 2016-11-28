{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Widget where

import Reflex.Dom hiding (select)
import Data.Default
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import qualified Data.Map as M
import JavaScript.JQuery hiding (Event)
import qualified Data.JSString as S
import qualified GHCJS.DOM.Types as GDT

import Card


performArg :: MonadWidget t m => (b -> IO a) -> Event t b -> m (Event t a)
performArg f x = performEvent (fmap (liftIO . f) x)

printOnChange :: (MonadWidget t m, Show a) => String -> Dynamic t a -> m ()
printOnChange prefix dyn = do
    performArg (\s -> putStrLn (prefix ++ ": " ++ show s)) (updated dyn)
    return ()

topWidget :: MonadWidget t m => m ()
topWidget = do
    divClass "container" $
        divClass "row" $
            elAttr "form" ("class" =: "col s9") $ do
                nw <- nameWidget
                dw <- descWidget
                fw <- flavorWidget
                sw <- cardsetWidget
                printOnChange "Card Name" nw
                printOnChange "Card Description" dw
                printOnChange "Card Flavor" fw
                printOnChange "Card Set" sw

    return ()

nameWidget :: MonadWidget t m => m (Dynamic t String)
nameWidget = do
    divClass "input-field col s4" $ do
        ti <- textInput $ def & textInputConfig_attributes .~ constDyn ("id" =: "cardname")
        elAttr "label" ("for" =: "cardname") (text "Card Name")
        return $ ti ^. textInput_value

descWidget :: MonadWidget t m => m (Dynamic t String)
descWidget = do
    divClass "input-field col s4" $ do
        ti <- textInput $ def & textInputConfig_attributes .~ constDyn ("id" =: "carddesc")
        elAttr "label" ("for" =: "carddesc") (text "Card Description")
        return $ ti ^. textInput_value

flavorWidget :: MonadWidget t m => m (Dynamic t String)
flavorWidget = do
    divClass "input-field col s4" $ do
        ti <- textInput $ def & textInputConfig_attributes .~ constDyn ("id" =: "cardflavor")
        elAttr "label" ("for" =: "cardflavor") (text "Card Flavor Text")
        return $ ti ^. textInput_value

cardsetWidget :: MonadWidget t m => m (Dynamic t String)
cardsetWidget =
    divClass "input-field col s7" $ do
        elAttr "select" ("multiple" =: "" <> "id" =: "cardset") $ do
            elAttr "option" ("value" =: "" <> "disabled" =: "" <> "selected" =: "")
                (text "None")
            elAttr "option" ("value" =: "Karazhan")
                (text "One Night in Karazhan")
        (el, _) <- elAttr' "div" ("id" =: "cardset-ob") $ text ""
        let ev = domEvent Click el
        ev' <- performArg (const $ getValue "#cardset") ev
        holdDyn "" ev'

getValue :: S.JSString -> IO String
getValue s = do
    jq <- select s
    jss <- getListOfString jq
    return $ S.unpack jss

foreign import javascript unsafe "$1.val().join(',')"
    getListOfString :: JQuery -> IO S.JSString
