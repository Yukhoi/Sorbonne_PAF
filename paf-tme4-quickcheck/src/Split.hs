module Split where

split :: Char -> String -> [String]
split breaker chaine = case break (== breaker) chaine of
    (x, breaker:ys) -> x : (split breaker ys) 
    (x, "") -> [x]


unsplit :: Char -> [String] -> String
unsplit connecter [] = []
unsplit connecter [x] = x
unsplit connecter (l:ls) = l ++ [connecter] ++ (unsplit connecter ls)


prop_split_unsplit :: Char -> String -> Bool
prop_split_unsplit c str = unsplit c (split c str) == str
