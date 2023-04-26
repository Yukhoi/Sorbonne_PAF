-- le module `Main` correspond au point d'entrée
-- des programmes exécutables Haskell (ghc).
module Main where

-- On utilise quelques outils du module d'entrées/sorties.
import System.IO (hFlush, stdout)

-- Un petit type somme pour les réponses
data Answer =
    Lower
    | Greater
    | Equal

message :: Answer -> String
message Lower = "Trop petit"
message Greater = "Trop grand"
message Equal = "Trouvé!"

-- Vérification des nombres candidats (à compléter)
checkGuess :: Integer -> Integer -> Answer
checkGuess secret guess = if secret > guess then Lower
                        else (if secret < guess then Greater
                        else Equal)

-- checkGuess 100 50 == Lower
-- checkGuess 100 150 == Greater
-- checkGuess 100 100 == Equal

-- Stratégie de raffinement des candidats (à compléter)
-- Une stratégie simple est la suivante :
-- si le candidat est plus petit, alors on prend la moyenne de celui et la borne superieur,
-- si c'est plus grand alors on prend la moyenne de celui et la borne inferieur,  (pour accélérer la recherche)
-- ===> une stratégie plus efficace serait la bienvenue, notamment en
--      introduisant une borne max pour le choix du nombre ...
refineGuess :: Integer -> Integer -> Integer -> Integer -> (Integer, Integer, Integer)
refineGuess secret guess lower upper 
  | guess < secret = ((guess + upper) `div` 2, guess, upper)
  | guess > secret = ((guess + lower) `div` 2, lower, guess)
  | otherwise = (guess,lower,upper)
                    

-- refineGuess 100 50 == 51
-- refineGuess 100 150 == 75
-- refineGuess 100 100 = 100

-- Pouvez-vous expliquer ce que fait cette fonction ? 
-- Reponce: Envoie nombre de fois que refineGuess avant de deviner
-- N'hésitez pas à consulter un manuel de Haskell comme :
--  * Learn you a Haskell for Great Good => http://learnyouahaskell.com/chapters
--  * ou le Haskell Wikibool => https://en.wikibooks.org/wiki/Haskell
findSecret :: Integer -> Integer -> Integer -> Integer -> Integer
findSecret secret initGuess = find initGuess 1
    where find guess nb min max =
            let (guess', min' , max') = refineGuess secret guess min max
            in if guess' == guess
               then nb
               else (find guess' (nb + 1) min' max')

-- findSecret 100 50 == 51
-- findSecret 100 150 == 27
-- findSecret 100 100 == 1


-- La boucle principale du jeu,
-- qui prend en entrée le secret à trouver ainsi
-- que le nombre de tentatives (en fait le rang de
-- la tentative actuelle).
-- On y reviendra mais le type de retour `IO Integer`
-- correspond à une action d'entrées/sorties qui,
-- une fois exécutée par le runtime, retourne un entier.
gameLoop :: Integer -> Integer -> Integer -> Integer -> IO Integer
gameLoop secret nb min max = do -- ceci permet de chaîner des actions d'entrées/sorties en séquence
  putStrLn ("Tentative #" ++ (show nb))    -- première action de la séquence
  putStr "Quel nombre ? "                  -- deuxième action
  hFlush stdout                            -- etc.
  -- on lit sur l'entrée standard et on récupère la valeur dans une variable `guessStr`
  guessStr <- getLine    -- ici c'est une action avec retour de valeur (une chaîne)

  if guessStr == "t"
  then do  -- triche
    putStrLn "Ok, je triche"
    pure $ findSecret secret ((min + max) `div` 2) min max 
  else do
    -- on transforme la chaîne en un entier
    let guess = read guessStr :: Integer  -- en Haskell il est fréquent de "caster" des expressions
                                        -- mais la sémantique est très différente de C ou C++
                                        -- c'est nécessaire ici car `read` est polymorphe (cf. typeclasses)
  -- Remarque : l'instruction `read` est *unsafe*, elle lance une exception si l'entrée ne correspond
  -- pas à un nombre... Rendre ce petit bout de code *safe* est une extension intéressante.

    -- on vérifie le candidat
    let answer = checkGuess  secret guess
    -- on affiche la réponse
    putStrLn (message answer)
    case answer of
      Equal -> pure nb  -- on retourne le nombre de tentatives, une valeur "pure", dans le cadre des entrées/sorties ("impures")
      _ -> gameLoop secret (nb+1) min max-- sinon on n'a pas encore trouvé alors on démarre une nouvelle tentative


startLoop :: Integer -> Integer -> IO Integer
startLoop min max = do
  putStr "Donne un secret: "
  hFlush stdout
  secretStr <- getLine
  let secret = read secretStr :: Integer
  if secret < min || secret > max 
    then do 
        putStrLn "Le secret donné doit être dans l'intervalle"
        startLoop min max
    else pure secret



-- le point d'entrée du programme
-- la valeur `()` s'appelle *unit* et est du type `()` (également *Unit*).
-- cela correspond à des programmes d'entrées/sorties.
main :: IO ()
main = do
  putStrLn "Devine le nombre!"
  putStrLn "================="
  putStrLn "  -> un super jeu de PAF!"
  -- le defil 1
  putStr "Donner l'intervalle pour le secret"
  putStr "Donner la borne minimal: "
  hFlush stdout
  minStr <- getLine
  putStr "Donner la borne maximal: "
  hFlush stdout
  maxStr <- getLine
  let min = read minStr :: Integer
  let max = read maxStr :: Integer
  secret <- startLoop min max -- ici on affiche 40 retours de ligne pour faire disparaître la saisie du nombre (ce n'est pas très "propre" ...)
  newlines 40
  putStrLn "Merci ! Le secret est enregistré."
  putStrLn "Maintenant votre adversaire va devoir le deviner ..."
  nb <- gameLoop secret 1 min max
  putStrLn ("Vous avez trouvé le secret en " ++ (show nb) ++ " tentative(s)")

-- une petite fonction auxiliaire pour ajouter des retours charriots.
newlines :: Int -> IO ()
newlines 0 = pure ()
newlines n = do
  putStrLn ""
  newlines (n-1)

randomNb :: Integer -> [Integer]
randomNb seed = iterate (\x -> (25210345917 * x + 11) `mod` (2^48)) seed
