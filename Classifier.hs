module Classifier(learn, classify, Model) where

import Prelude hiding (mapM)

import Control.Monad (forever)

import Data.Traversable (mapM)
import Data.Ratio ((%))
import Data.Char (toLower)
import Data.List (sortBy, tails)
import Data.Ord (comparing)
import qualified Data.Map as M
import qualified Data.Set as S

type Model = M.Map String Rational

-- | N-Grams of a list.
ngram :: Int -> [a] -> [[a]]
ngram n = takeWhile ((==n) . length) . map (take n) . tails

-- | N-Grams with particular N fixed.
grams :: [a] -> [[a]]
grams = ngram 3

-- | Mark the begining and end of a string by the `'\0'` character.
mark :: String -> String
mark s = "\0" ++ s ++ "\0"

-- | Preprocess a string, cleaning it for further processing.
preprocess :: String -> String
preprocess = mark . map toLower

-- | Add one occurence of a specific key to an occurence counting mapping.
addOne :: Ord a => a -> M.Map a Integer -> M.Map a Integer
addOne = M.alter f
  where
    f (Just i) = Just (i + 1)
    f Nothing  = Just 1

-- | Add one occurence of a specific key to an occurence counting mapping, aswell as to a counter.
addCount :: Ord a => a -> (Integer, M.Map a Integer) -> (Integer, M.Map a Integer)
addCount x (j, n) = (j + 1, addOne x n)

-- | Add the contributions of a word.
addWord :: String -> (Integer, M.Map String Integer) -> (Integer, M.Map String Integer)
addWord s (i, m) = foldr addCount (i, m) $ grams s

-- | Given a list of strings, builds a language model.
buildLM :: [String] -> (Integer, M.Map String Integer)
buildLM = foldr addWord (0, M.empty)

-- | Given a set of occuring elements, add one count to each. Known as "add-1 smoothing.".
smoothing :: S.Set String -> (Integer, M.Map String Integer) -> (Integer, M.Map String Integer)
smoothing ss m = S.fold addCount m ss

-- | Given a counter and an occurence mapping, builds a model. 
scale :: (Integer, M.Map String Integer) -> Model
scale (i, m) = fmap (%i) m

-- | Given a map of annotated file paths, build a map of annotated models.
learn :: M.Map a FilePath -> IO (M.Map a Model)
learn cs = do
    lms <- mapM processFile cs
    let grs = S.unions $ map (M.keysSet . snd) $ M.elems lms
    return $ fmap (scale . smoothing grs) lms 

-- | Build a counter and a occurence mapping from a given file, countaing one word per line.
processFile :: FilePath -> IO (Integer, M.Map String Integer)
processFile fp = fmap (buildLM . map preprocess . lines) $ readFile fp

-- | Gets the score of a particular ngram in the model.
gramScore :: Model -> String -> Rational
gramScore m s = case M.lookup s m of
    Just x -> x
    Nothing -> 1

-- | Gets the score of a particular word in the model.
wordScore :: String -> Model -> Rational
wordScore s m = product $ map (gramScore m) $ grams $ preprocess s

-- | Given a word and a list of ranked answers, produce a human comprehensible answer. 
answer :: String -> [(String, Rational)] -> String
answer w ((ta, sa):(tb, sb):(tc, sc):_)
    | sa == sc     = "Absolutely no idea on what " ++ w ++ " might be..."
    | sa <= 2 * sc = "Wild guess: " ++ w ++ " is " ++ ta ++ ", right?"
    | sa <= 4 * sb = w ++ " might be " ++ ta ++ ", or perhaps " ++ tb ++ "."
    | sa <= 8 * sb = "Pretty sure " ++ w ++ " is " ++ ta ++ "."
    | otherwise    = w ++ " surely is " ++ ta ++ "."

-- | Given a word and some models, gets a human readable classification of the word.
classify :: String -> M.Map String Model -> String
classify word = answer word . sortBy (comparing ((0-) . snd)) . M.toList . fmap (wordScore word)
