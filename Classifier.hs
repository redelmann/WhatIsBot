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

ngram :: Int -> [a] -> [[a]]
ngram n = takeWhile ((==n) . length) . map (take n) . tails

grams :: [a] -> [[a]]
grams = ngram 3

mark :: String -> String
mark s = "\0" ++ s ++ "\0"

preprocess :: String -> String
preprocess = mark . map toLower

addOne :: Ord a => a -> M.Map a Integer -> M.Map a Integer
addOne = M.alter f
  where
    f (Just i) = Just (i + 1)
    f Nothing  = Just 1

addCount :: Ord a => a -> (Integer, M.Map a Integer) -> (Integer, M.Map a Integer)
addCount x (j, n) = (j + 1, addOne x n)

addWord :: String -> (Integer, M.Map String Integer) -> (Integer, M.Map String Integer)
addWord s (i, m) = foldr addCount (i, m) $ grams s

buildLM :: [String] -> (Integer, M.Map String Integer)
buildLM = foldr addWord (0, M.empty)

smoothing :: S.Set String -> (Integer, M.Map String Integer) -> (Integer, M.Map String Integer)
smoothing ss m = S.fold addCount m ss

probabilities :: (Integer, M.Map String Integer) -> Model
probabilities (i, m) = fmap (%i) m

learn :: M.Map String FilePath -> IO (M.Map String Model)
learn cs = do
    lms <- mapM processFile cs
    let grs = S.unions $ map (M.keysSet . snd) $ M.elems lms
    return $ fmap (probabilities . smoothing grs) lms 

processFile :: FilePath -> IO (Integer, M.Map String Integer)
processFile fp = fmap (buildLM . map preprocess . lines) $ readFile fp

gramScore :: Model -> String -> Rational
gramScore m s = case M.lookup s m of
    Just x -> x
    Nothing -> 1

wordScore :: String -> Model -> Rational
wordScore s m = product $ map (gramScore m) $ grams $ preprocess s

answer :: String -> [(String, Rational)] -> String
answer w ((ta, sa):(tb, sb):(tc, sc):_)
    | sa == sc = "Absolutely no idea on what " ++ w ++ " might be..."
    | sa <= 2 * sc = "Wild guess: " ++ w ++ " is " ++ ta ++ ", right?"
    | sa <= 4 * sb = w ++ " might be " ++ ta ++ ", or perhaps " ++ tb ++ "."
    | sa <= 8 * sb = "Pretty sure " ++ w ++ " is " ++ ta ++ "."
    | otherwise = w ++ " surely is " ++ ta ++ "."

classify :: String -> M.Map String Model -> String
classify word = answer word . sortBy (comparing ((1/) . snd)) . M.toList . fmap (wordScore word)
