{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Text.HTML.Scalpel

type Title = String
type SubTitle = String
type Synopsis = String

data Programme =  Programme { title :: Title
                            , subtitle :: SubTitle
                            , synopsis :: Synopsis
                            , thumbnail :: String
                            , url :: String
                            , index :: Int
                            , available :: String
                            , duration :: String } deriving (Show)


someFunc :: IO ()
someFunc = putStrLn "someFunc"

otherFunc = do
  titles <- allTitles
  maybe printError printTitles titles
  where
    printError = putStrLn "ERROR: could not scrape URL."
    printTitles = mapM_ putStrLn

allTitles :: IO (Maybe [String])
allTitles = scrapeURL "https://www.bbc.co.uk/iplayer/categories/films/a-z?sort=atoz&page=1" titles
  where
    titles :: Scraper String [String]
    titles = chroots ("div" @: [hasClass "content-item"]) title

    title :: Scraper String String
    title  = do
      progtitle <- text $ "div" @: [hasClass "content-item__title"]
      return progtitle
