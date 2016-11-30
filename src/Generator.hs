{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- generate Card database to JSON
module Generator where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy.Char8 as LBS

import Control.DeepSeq
import Control.Seq
import Control.Arrow ((&&&))
import Text.XML.Cursor
import Text.HTML.DOM
import Network.Wreq
import Control.Lens.Operators
import Data.Aeson.Lens
import Data.Aeson
import Control.Concurrent.MVar
import Control.Monad
import Control.Exception
import Data.Either
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.MSem
import Data.Monoid ((<>))

import Card
import Parser

hearthstoneApiUrl :: String
hearthstoneApiUrl = "https://api.hearthstonejson.com/v1/latest/enUS/cards.json"

-- extract full card list from HearthStone API project
allCardList :: IO [T.Text]
allCardList = do
    r <- get hearthstoneApiUrl
    return $ r ^.. responseBody . _Array . traverse . key "name" . _String

allCardListLocal :: IO [T.Text]
allCardListLocal = do
    Just (r :: Value) <- decode <$> LBS.readFile "cards.json"
    return $ r ^.. _Array . traverse . key "name" . _String

getCard :: T.Text -> MVar Int -> IO (Either T.Text ([T.Text], Card))
getCard name cnt = do
    modifyMVar_ cnt (return . (+1))
    let handler (e :: SomeException) = return . Left $ name <> ": " <> T.pack (take 100 $ show e)
    strictCatch (Right <$> parseLBSCard name) handler

getAllCards :: [T.Text] -> IO [([T.Text], Card)]
getAllCards cl = do
    let tot = length cl
    putStrLn $ "Total: " ++ show tot
    putStrLn "getting detailed card info from Wiki"
    counter <- newMVar (0 :: Int)
    forkIO $ reportProgress counter tot
    result <- forPool 10 cl (`getCard` counter)
    let result' = rights result
    threadDelay 100000
    putStrLn "\nDone"
    putStrLn $ show (length result') ++ " out of " ++ show tot ++ " completed!"
    let failed = lefts result
    unless (null failed) $ do
        putStrLn "The following 'cards' failed:"
        mapM_ T.putStrLn (lefts result)
    return result'

processCardData :: [([T.Text], Card)] -> ([Card], [Image])
processCardData cs =
    let tups = map (fst &&& snd) cs
        tups' = map (\(url, c) -> (c, (c ^. cardName, url))) tups
    in (map fst tups', map snd tups')

reportProgress :: MVar Int -> Int -> IO ()
reportProgress m tot = do
    m' <- readMVar m
    when (m' < tot - 1) $ do
        putStr $ "\r\ESC[K" ++ show m' ++ "/" ++ show tot
        threadDelay 300000
        reportProgress m tot

mapPool :: Traversable t => Int -> (a -> IO b) -> t a -> IO (t b)
mapPool ma f xs = do
    sem <- new ma
    mapConcurrently (with sem . f) xs

forPool :: Traversable t => Int -> t a -> (a -> IO b) -> IO (t b)
forPool ma = flip (mapPool ma)

toNF :: (NFData a) => a -> IO a
toNF = evaluate . withStrategy rdeepseq

strictCatch :: (NFData a, Exception e) => IO a -> (e -> IO a) -> IO a
strictCatch = catch . (toNF =<<)
