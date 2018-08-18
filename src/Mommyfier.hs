module Mommyfier
( charToMommy
, isVowel
, stringToMommyfied
, removeAdjacentMommy
, mommyfy
) where

isVowel :: Char -> Bool
isVowel a
    | a `elem` ['a', 'e', 'i', 'o', 'u'] = True
    | otherwise = False

charToMommy :: Char -> String
charToMommy a
    | isVowel a = "mommy"
    | otherwise = a : ""

stringToMommyfied :: String -> [String]
stringToMommyfied ""     = []
stringToMommyfied (x:xs) = charToMommy x : stringToMommyfied xs

removeAdjacentMommy :: [String] -> [String]
removeAdjacentMommy [] = []
removeAdjacentMommy [x] = [x]
removeAdjacentMommy (x:xx:xs) =
    if x == "mommy" && xx == "mommy"
        then removeAdjacentMommy (x : xs)
        else x : removeAdjacentMommy (xx : xs)

mommyfy :: String -> String
mommyfy = concat . removeAdjacentMommy . stringToMommyfied
