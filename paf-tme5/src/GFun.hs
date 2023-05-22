module GFun where

data Series a = Zero a (Series a)

-- >>> :t Zero
-- Zero :: forall a. a -> Series a -> Series a

ones :: Series Integer 
ones = Zero 1 ones

takeSeries :: Int -> Series a -> [a]
takeSeries 0 _ = []
takeSeries x (Zero a rest) 
    | x > 0     = a : takeSeries (x-1) rest
    | otherwise = error "cannot take a negative number of element from series"

showSeries :: (Show a) => Series a -> String
showSeries s = foldl (\str elem -> if fst elem < 10 then 
                                        str ++ show (snd elem) ++ ".z^" ++ show (fst elem) ++ " + " 
                                    else str ++ "...") 
                    "" 
                    (zip [0..11] (takeSeries 11 s))

instance (Show a) => Show (Series a) where
    show = showSeries

applyFunc :: (a -> b) -> Series a -> Series b
applyFunc f (Zero a rest) = Zero (f a) (applyFunc f rest)

instance Functor Series where
    fmap = applyFunc

instance (Num a) => Num (Series a) where
    (Zero c1 s1) + (Zero c2 s2) = Zero (c1 + c2) (s1 + s2)
    (Zero c1 s1) - (Zero c2 s2) = Zero (c1 - c2) (s1 - s2)
    (Zero c1 s1) * (Zero c2 s2) = Zero (c1 * c2) (applyFunc (*c1) s2 + applyFunc (*c2) s1 + Zero 0 (s1 * s2))
    abs (Zero c1 s1) = Zero (abs c1) (abs s1)
    signum (Zero c1 s1) = Zero (signum c1) (signum s1)
    fromInteger i = Zero (fromInteger i) (fromInteger i)

instance (Eq a) => Eq (Series a) where
  (Zero c1 s1) == (Zero c2 s2) = c1 == c2

derive :: Num a => Series a -> Series a
derive (Zero a rest) = helper (Zero a rest) 1 where
    helper (Zero c s) counter = Zero (c * counter) (helper s (counter + 1))
