
module Main(main) where

import Classifier
import Data.Map (fromList)
import Control.Monad (forM_)
import Data.Functor ((<$>))

main :: IO ()
main = do
    models <- learn $ fromList 
        [ ("a Pokemon", "resources/pokemons1st.txt")
        , ("a pharmaceutical product", "resources/pharma.txt")
        , ("an ancient historian", "resources/historians.txt") ]
    ws <- lines <$> getContents
    forM_ ws $ \ word -> putStrLn $ classify word models
