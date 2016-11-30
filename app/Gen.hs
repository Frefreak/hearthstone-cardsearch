module Main where

import Control.Concurrent
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import System.IO
import Data.List (nub)

import Generator

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "getting card name list from HearthStone API"
    tid <- forkIO $ forever (putStr "." >> threadDelay 1000000)
    cardlist <- allCardList
    killThread tid
    putStrLn "\nDone"
    cardData <- getAllCards $ nub cardlist
    let (cards, imgs) = processCardData cardData
        cards' = encode cards
        imgs' = encode imgs
    LBS.writeFile "card-data.json" cards'
    LBS.writeFile "card-image.json" imgs'
    hSetBuffering stdout LineBuffering


