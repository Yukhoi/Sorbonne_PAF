module Lib
    ( someFunc
    , maxInt
    , fibo
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

maxInt :: Integer -> Integer -> Integer
maxInt a b = if a>b then a else b

fibo :: Integer -> Integer
fibo a = if a==0 then 1 else if a == 1 then 1
    else (fibo(a-1)) + (fibo(a-2))
