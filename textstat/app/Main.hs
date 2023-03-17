-- Option de compilation pour que les chaînes litérales
-- soient surchargées plutôt que fixées à `[Char]`
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- Le type `Text` est disponible
import Data.Text (Text)

-- Les fonctions de manipulation de textes
-- sont préfixées par `T` plutôt que `Data.Text`

import qualified Data.Text as T

-- Les fonctions d'entrées sorties pour les textes

import qualified Data.Text.IO as TIO

import Stats

import Data.List

import System.Environment

analyse :: Text -> String -> IO()
analyse texte titre = (putStrLn ("Analyse de texte de:" ++ titre ++ "\n"))
                        >>=(\_ -> putStrLn ("Nombre de caractères: " ++ (show (T.length texte)) ++ "\n"))
                        >>=(\_ -> putStrLn ("Nombre de mots" ++ (show (compteMot texte)) ++ "\n"))
--                        >>= let dict = consDict texte in
--                            (\_ -> putStrLn("*** Caracteres de " ++ titre ++ "***\n\n" ++ (imprimDict (triDict dict)) ))

mainEtParse :: [String] -> IO()
mainEtParse [] = putStrLn "Il manque argument.\n"
mainEtParse (titre:_) = (TIO.readFile ("./" ++ titre)) >>= (\t -> analyse t titre)


main :: IO ()
main = do
    getArgs >>= mainEtParse

