module Poly where

import Data.Map (Map, fromList, unionWith, filter, findMax, foldrWithKey, toList, map)
import qualified Data.Map as Map

import Data.List (iterate)
import qualified Data.List as List

data Polynomial a = Null | Polynomial (Int, Map Int a)

validPolynomial :: (Eq a, Num a) => Polynomial a -> Bool
validPolynomial Null = True
validPolynomial (Polynomial (degree, map)) = fst (findMax map) == degree && snd (findMax map) /= 0

displayPolynomial :: (Show a, Eq a, Num a) => Polynomial a -> String
displayPolynomial Null = "null"
displayPolynomial (Polynomial (degree, map)) = foldrWithKey (\key value string -> string ++ show value ++ (if key > 0 then ".x^" ++ show key ++ " + " else "")) "" map

instance (Show a, Num a, Eq a) => Show (Polynomial a) where
    show = displayPolynomial

example :: Polynomial Int
example = Polynomial (4, fromList [(0, 4), (1, 0), (2, 2), (3, 0), (4, 1)])

example' :: Polynomial Int
example' = Polynomial (4, fromList [(0, 0), (1, 0), (2, 2), (3, 0), (5, 1)])

example'' :: Polynomial Int
example'' = Polynomial (4, fromList [(0, 0), (1, 0), (2, 2), (3, 0), (4, 0)])

normalize :: (Eq a, Num a) => Polynomial a -> Polynomial a
normalize Null = Null
normalize (Polynomial (degree, map)) =
    let map' = Map.filter ( /= 0) map in
        Polynomial (fst (findMax map'), map')

addPolynomials :: (Eq a, Num a) => Polynomial a -> Polynomial a -> Polynomial a
addPolynomials Null (Polynomial poly) = Polynomial poly
addPolynomials (Polynomial poly) Null = Polynomial poly
addPolynomials (Polynomial (d1, map1)) (Polynomial (d2, map2)) =
    let map3 = unionWith (+) map1 map2
        in normalize $ Polynomial (0, map3)

example2 :: Polynomial Int
example2 = Polynomial (5, fromList [(0, 2), (1, 0), (2, 3), (3, 5), (4, 1), (5, 1)])

mapPoly :: (a -> b) -> Polynomial a -> Polynomial b
mapPoly func Null = Null
mapPoly func (Polynomial (degree, map)) = Polynomial (degree, Map.map func map)

negatePolynomial :: (Eq a, Num a) => Polynomial a -> Polynomial a
negatePolynomial = mapPoly negate

subtractPolynomials :: (Eq a, Num a) => Polynomial a -> Polynomial a -> Polynomial a
subtractPolynomials Null (Polynomial poly) = negatePolynomial (Polynomial poly)
subtractPolynomials (Polynomial poly) Null = Polynomial poly
subtractPolynomials (Polynomial (d1, map1)) (Polynomial (d2, map2)) =
    let map3 = unionWith (-) map1 map2
        in normalize $ Polynomial (0, map3)

getCombinations :: Map Int a -> Map Int a -> [((Int, a), (Int, a))]
getCombinations map1 map2 = [(x, y) | x <- toList map1, y <- toList map2 ]

multiply :: (Eq a, Num a) => [((Int, a), (Int, a))] -> [(Int, a)]
multiply = List.map (\element -> (fst (fst element) + fst (snd element), snd (fst element) * snd (snd element)))

multiplyPolynomials :: (Eq a, Num a) => Polynomial a -> Polynomial a -> Polynomial a
multiplyPolynomials Null _ = Null
multiplyPolynomials _ Null = Null
multiplyPolynomials (Polynomial (d1, map1)) (Polynomial (d2, map2)) = 
    normalize (Polynomial (d1 + d2, 
                    Map.fromListWith (+) (multiply $ getCombinations map1 map2) ))

absolutePolynomial :: (Eq a, Num a) => Polynomial a -> Polynomial a
absolutePolynomial = mapPoly abs

signumPolynomial :: (Eq a, Num a) => Polynomial a -> Polynomial a
signumPolynomial = id

fromIntegerToPolynomial :: (Eq a, Num a) => Integer -> Polynomial a
fromIntegerToPolynomial x 
    | x >= 0 = Polynomial (fromIntegral x, fromList (zip (Prelude.take (fromIntegral x+1) (iterate (+1) 0)) (Prelude.take (fromIntegral x+1) (repeat 1)) ) )
    | otherwise = error "cannot create polynome with a negative degree"

instance (Eq a, Num a) => Num (Polynomial a) where
  (+) = addPolynomials
  (*) = multiplyPolynomials
  abs = absolutePolynomial
  signum = signumPolynomial
  fromInteger = fromIntegerToPolynomial
  negate = negatePolynomial

deriveMap ::  Map Int Int -> Map Int Int
deriveMap map = Map.fromList (Map.foldrWithKey (\key value res -> if key > 0 then (key-1, value*key) : res else res ) [] map)

derive :: Polynomial Int -> Polynomial Int
derive Null = Null
derive (Polynomial (degree, map)) = normalize (Polynomial (degree-1, deriveMap map))
