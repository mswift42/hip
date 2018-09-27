{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( otherFunc,
      showFilms
    ) where

import Text.HTML.Scalpel
import Text.Show.Functions

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
                            , duration :: String } deriving (Show, Eq)


someFunc :: IO ()
someFunc = putStrLn "someFunc"

otherFunc :: IO ()
otherFunc = do
  titles <- allTitles
  maybe printError printTitles titles
  where
    printError = putStrLn "ERROR: could not scrape URL."
    printTitles = mapM_ putStrLn

showFilms :: IO ()
showFilms = do
  programmes <- films
  maybe printError printFilms programmes
  where
    printError = putStrLn "Error: could not scrape URL."
    printFilms = mapM_ print

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
    programmes :: Scraper String [Programme]
    programmes = chroots ("div" @: [hasClass "content-item"]) programme

    programme :: Scraper String Programme
    programme = do
      title <- text $ "div" @: [hasClass "content-item__title"]
      subtitle <- text $ "div" @: [hasClass "content-item__description"]
      synopsis <- text $ "div" @: [hasClass "content-item__description" ]
      thumbnail <- attr "srcset" $ "source" 
      url <- attr "href" $ "a"
      -- index <- 0
      available <- text $ "div" @: [hasClass "content-item__sublabels"] 
      duration <- text $ "div" @: [hasClass "content-item__labels"] 
      return $ Programme title subtitle synopsis thumbnail url 0 available duration
