module Stats
    ( someFunc
    , compteMot
    , compteLigne
    , compteLettre
    , compteLettre2
    , listFrequence
    , zipFrequence
    , printList
    ) where

import Data.Text (Text)

import Data.List (sortOn)

import qualified Data.Text as T


someFunc :: IO ()
someFunc = putStrLn "someFunc"



{-compteMot :: T.Text -> Integer
compteMot texte = case T.uncons texte of
    Nothing -> 0
    Just _ -> foldl (\x _ ->  x + 1) 0 (T.words texte)-}

compteMot :: Text -> Int 
compteMot = T.foldl' (\x w -> if w == ' ' then x + 1 else x) 0

compteLigne :: Text -> Int 
compteLigne = T.foldl' (\x w -> if w == '\n' then x + 1 else x) 1

compteLettre :: Char -> Text -> Int 
compteLettre letter = T.foldl' (\x w -> if w ==letter then x + 1 else x) 0

compteLettre2 :: Char -> (Text -> Int)
compteLettre2 c  = compteLettre c

listFrequence :: [Text -> Int] -> Text -> [Int]
listFrequence [] _ = []
listFrequence (f : l) t = (f t): listFrequence l t

zipFrequence :: Text -> [(Char,Int)]
zipFrequence t = take 5 . reverse . sortOn snd $ zip ['a'..'z'] $ listFrequence (map compteLettre2 ['a'..'z']) t

printList :: [(Char, Int)] -> String
printList [] = []
printList ((c, i):frs) = ((c:[]) ++ ": " ++ (show i) ++ "\n") ++ printList frs



