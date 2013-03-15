module Main(main) where

import Control.Monad (forM_)

import Data.Map (fromList)
import Data.Functor ((<$>))

import Classifier

-- | Reads user names from stdin and prints out their classification to sdtout.
main :: IO ()
main = do
    models <- learn $ fromList 
        [ ("a Pokemon", "resources/pokemons1st.txt")
        , ("a pharmaceutical product", "resources/pharma.txt")
        , ("an ancient historian", "resources/historians.txt") ]
    ws <- lines <$> getContents
    forM_ ws $ \ word -> putStrLn $ classify word models
