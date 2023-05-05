module Split where

-- split :: Char -> String -> [String]
-- split breaker "" = []
-- --split breaker [breaker] = [breaker] 
-- split breaker chaine = case break (== breaker) chaine of
--     (x, y:ys) -> x : (split breaker ys) 
--     (x, "") -> [x]

split :: Char -> String -> [String]
split c str = case break (== c) str of
    (x , c:xs) -> x : split c xs
    (x , "") -> [x] 
    
-- break (==a) banana =  b anana
-- 'a' "a" =>  ["a"]

unsplit :: Char -> [String] -> String
unsplit c [] = [] 
unsplit c [x] = x
unsplit c (x:xs) = x ++ (c :(unsplit c xs)) 
-- unsplit :: Char -> [String] -> String
-- unsplit connecter [] = []
-- unsplit connecter [x] = x
-- unsplit connecter (l:ls) = l ++ [connecter] ++ (unsplit connecter ls)
-- -- unsplit connecter list = case (connecter, list) of
-- --     (x, l:ls) -> l ++ [x] ++ (unsplit x ls)
-- --     (x, []) -> []

-- '/' ["aa", "bb", "ccc", "dd d"]  ==> "aa/bb/ccc/dd d"
-- 'a' ["a"]



prop_split_unsplit :: Char -> String -> Bool
prop_split_unsplit c str = unsplit c (split c str) == str
