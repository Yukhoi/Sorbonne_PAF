module Stats
    ( someFunc
    , compteMot
    ) where

import Data.Text (Text)

import qualified Data.Text as T


someFunc :: IO ()
someFunc = putStrLn "someFunc"



compteMot :: T.Text -> Integer
compteMot texte = case T.uncons texte of
    Nothing -> 0
    Just _ -> foldl (\x _ ->  x + 1) 0 (T.words texte)

