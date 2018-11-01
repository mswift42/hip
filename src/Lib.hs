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

type BeebURL = String
type TestHTMLUrl = String

data IplayerURL = BeebURL | TestHTMLUrl



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
  print "Printing Films"
  maybe printError printFilms programmes
  where
    printError = putStrLn "Error: could not scrape URL."
    printFilms = mapM_ print

allTitles :: IO (Maybe [Title])
allTitles = scrapeURL "https://www.bbc.co.uk/iplayer/categories/films/a-z?sort=atoz&page=1" titles
  where
    titles :: Scraper String [Title]
    titles = chroots ("div" @: [hasClass "content-item"]) programmeTitle



films :: IO (Maybe [Programme])
films = scrapeURL "https://www.bbc.co.uk/iplayer/categories/films/a-z?sort=atoz&page=1" programmes
  where
    programmes :: Scraper String [Programme]
    programmes = chroots ("div" @: [hasClass "content-item"]) programme

    programme :: Scraper String Programme
    programme = do
      title <- programmeTitle
      subtitle <- programmeSubTitle
      synopsis <- text $ "div" @: [hasClass "content-item__info__secondary" ] // "div" @: [hasClass "content-item__description"]
      thumbnail <- attr "srcset" "source"
      url <- attr "href" "a"
      available <- texts $  "div" @: [hasClass "content-item__sublabels"] // "span"
      duration <-  texts $  "div" @: [hasClass "content-item__sublabels"] // "span"
      return $ Programme title subtitle synopsis thumbnail url 0 (available !! 1) (head duration)

programmeTitle :: Scraper String Title
programmeTitle = text $ "div" @: [hasClass "content-item__title"]

programmeSubTitle :: Scraper String SubTitle
programmeSubTitle = text $ "div" @: [hasClass "content-item__primary"] // "div" @: [hasClass "content-item__description"]

sublabeltexts :: Scraper String [String]
sublabeltexts = texts $ "div" @: [hasClass "content-item__sublabels"] // "span"