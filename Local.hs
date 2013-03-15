module Main(main) where

import System.FilePath

import Control.Monad (forM_)

import Data.Monoid (mempty)
import Data.Map (fromList)
import Data.Functor ((<$>))

import Options.Applicative

import Classifier

data Options = Options { path :: FilePath }

-- | Parser for the options.
optionsParser :: Parser Options
optionsParser = Options <$> strOption (short 'd' 
                            <> long "directory"
                            <> help "Directory in which resources files are located"
                            <> value "resources"
                            <> metavar "DIR")

-- | Reads user names from stdin and prints out their classification to sdtout.
main :: IO ()
main = execParser opts >>= \ os -> do
    let p = path os
    models <- learn $ fromList 
        [ ("a Pokemon", p </> "pokemons1st.txt")
        , ("a pharmaceutical product", p </> "pharma.txt")
        , ("an ancient historian", p </> "historians.txt") ]
    ws <- lines <$> getContents
    forM_ ws $ \ word -> putStrLn $ classify word models
  where
    opts = (info (optionsParser <**> helper) mempty)
