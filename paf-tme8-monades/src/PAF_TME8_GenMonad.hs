
module PAF_TME8_GenMonad where

{-{

# TME 8 : Générateur aléatoire monadique de QuickCheck

}-}

-- pure == return 

import Test.QuickCheck
import System.Random (Random)
import Control.Applicative (liftA2)

samples :: Int -> Gen a -> IO [a]
samples n gen = do
  l <- sample' gen
  return $ take n l

chooseNat :: (Random a, Num a) => a -> Gen a
chooseNat n = choose (1, n)

genFreq :: Gen Integer
genFreq = frequency [(60, choose (1, 10)), (30, choose (100, 110)), (10, choose (1000, 1010))]

freqStats :: (Num a, Ord a) => [a] -> (Double, Double, Double)
freqStats xs =
  aux 0 0 0 xs
  where aux nb1 nb2 nb3 [] =
          let tot = nb1 + nb2 + nb3
              coef = 100.0 / (fromIntegral tot)
          in (fromIntegral nb1 * coef, fromIntegral nb2 * coef, fromIntegral nb3 * coef)
        aux nb1 nb2 nb3 (x:xs) | x <= 10 = aux (nb1+1) nb2 nb3 xs
                               | x <= 110 = aux nb1 (nb2+1) nb3 xs
                               | otherwise = aux nb1 nb2 (nb3+1) xs

checkFrequency :: (Random a, Num a, Ord a) => Gen [a] -> IO (Double, Double, Double) 
checkFrequency gen = do
  xs <- generate gen
  return $ freqStats xs

chooseInv :: Num a => Gen a -> Gen a
chooseInv gen = gen >>= (\x -> return (- x)) 

chooseInv2 :: Num a => Gen a -> Gen a
chooseInv2 gen = do
  x <- gen
  return (-x)

genPair :: Gen a -> Gen b -> Gen (a, b)
genPair genFst genSnd = do
  x <- genFst
  y <- genSnd
  return (x,y)

-- comme le do, mais faire en applicatif <$>  et <*>
-- genPair2 :: Applicative f => f a -> f b -> f (a, b) 
genPair2 :: Gen a -> Gen b -> Gen (a, b)
genPair2 genFst genSnd = genFst >>= (\x -> genSnd >>= (\y -> pure (x, y)))

data Personne = Personne { nom :: String, prenom :: String, age :: Int }
  deriving (Show, Eq)

genPersonne1 :: Gen Personne
genPersonne1 = Personne <$> vectorOf 10 (choose ('a', 'z')) <*> vectorOf 10 (choose ('a', 'z')) <*> choose (7,99)

-- genPersonne2 :: Gen Personne
-- genPersonne2 =Personne <$> resize 10 $ listOf $ choose ('a', 'z')
--                        <*> resize 10 $ listOf $ choose ('a', 'z')
--                        <*> choose (7, 99)

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe gen = do
  x <- arbitrary -- ici on tire un booléen
  case x of
    False -> return Nothing
    True -> gen >>= (pure . Just)  -- ou  do { x <- gen ; pure (Just x) }

data Geom = Point
  | Rect { longueur :: Int, largeur :: Int }
  | Circle { rayon :: Int }
  deriving (Show, Eq)

genGeom :: Gen Geom
genGeom =  frequency [ (20, pure Point),
                      (50, uncurry Rect <$> (do
                                                l <- choose (5,60)
                                                w <- choose (1,40) `suchThat` (<= l)
                                                return (l,w)) ),
                      (30, Circle <$> choose (1,10))
                    ]

data Nat = Z | S Nat
  deriving (Show, Eq, Ord)

genNat :: Gen Nat
genNat = do
  m <- getSize  -- on utilise le paramètre de taille comme borne max
  n <- choose (1, m)
  genAux n Z
    where genAux :: Int -> Nat -> Gen Nat
          genAux n k | n > 0 = genAux (n-1) (S k)
                     | otherwise = return k

listOfSize :: Gen a -> Int -> Gen [a]
listOfSize _ _ = loop size
                        where loop cpt 
                                | cpt <= 0 = pure []
                                | otherwise =  liftA2 (:) gen (loop (cpt - 1)) 

sizedList :: Gen a -> Gen [a]
sizedList gen = do
  size <- getSize
  listOfSize gen size

sizedList' :: Gen a -> Gen [a]
sizedList' gen = sized (listOfSize gen) 


data BinTree a = Tip | Node a (BinTree a) (BinTree a)
  deriving (Show, Eq)

genBinTreeNaive :: Gen a -> Int -> Gen (BinTree a)
genBinTreeNaive _ 0 = return Tip
genBinTreeNaive gen 1 = gen >>= \x -> return $ Node x Tip Tip
genBinTreeNaive gen size = do
  x <- gen
  lsize <- choose (0, size-1)
  l <- genBinTreeNaive gen lsize
  r <- genBinTreeNaive gen (size - lsize)
  return $ Node x l r

genBinTree :: Gen a -> Gen (BinTree a)
genBinTree gen = do
                  n <- getSize
                  genBinTreeNaive gen n 

