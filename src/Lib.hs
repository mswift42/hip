{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( otherFunc
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

otherFunc :: IO ()
otherFunc = do
  titles <- allTitles
  maybe printError printTitles titles
  where
    printError = putStrLn "ERROR: could not scrape URL."
    printTitles = mapM_ putStrLn

allTitles :: IO (Maybe [Title])
allTitles = scrapeURL "https://www.bbc.co.uk/iplayer/categories/films/a-z?sort=atoz&page=1" titles
  where
    titles :: Scraper String [Title]
    titles = chroots ("div" @: [hasClass "content-item"]) title

    title :: Scraper String Title
    title  = do
      progtitle <- text $ "div" @: [hasClass "content-item__title"]
      return progtitle


films :: IO (Maybe [Programme])
films = scrapeURL "https://www.bbc.co.uk/iplayer/categories/films/a-z?sort=atoz&page=1" programmes
  where
    programmes :: Scraper String [Title]
    programmes = chroots ("div" @: [hasClass "content-item"]) programme

    programme :: Scraper String Programme
    programme = do
      title <- text $ "div" @: [hasClass "content-item__title"]
      subtitle <- text $ "div" @: [hasClass "content-item__description"]
      synopsis <- "synopsis"
      thumbnail <- attr "srcset" $ "source" 
      url <- attr "href" $ "a"
      index <- 0
      available <- text $ "div" @: [hasClass "content-item__sublabels"] 
      duration <- "a month"
      return $ Programme title subtitle synopsis thumbnail url index available duration
