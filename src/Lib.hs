module Lib
    ( someFunc
    ) where

import Text.HTML.Scalpel

data Programme =  Programme { title :: String
                            , subtitle :: String
                            , synopsis :: String
                            , thumbnail :: String
                            , url :: String
                            , index :: Int
                            , available :: String
                            , duration :: String } deriving (Show)


someFunc :: IO ()
someFunc = putStrLn "someFunc"
