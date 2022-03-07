module Hangman where

import Data.List (permutations)
import Data.Char

type LetterBoard = String

setupLetterBoard :: String -> LetterBoard
setupLetterBoard [] = []
setupLetterBoard (x : xs) = "*" ++ setupLetterBoard xs

promptGuess :: IO Char
promptGuess = do
  putStrLn "Guess a letter:"
  char <- getChar
  pure char

checkIfWon :: String -> String -> Bool
checkIfWon x y = 
    if x == y then
        True
    else
        False


updateGuessList :: String -> String -> Char -> String
updateGuessList = undefined

updateLetterBoard :: String -> String -> Char -> String
updateLetterBoard (x : xs) (y : ys) guess =
    if x == y then -- letter has been guessed
        [y] ++ updateLetterBoard xs ys guess
    else if x == guess then -- letter hasn't been guessed, but matches gamePhrase letter
        [guess] ++ updateLetterBoard xs ys guess
    else
        "*" ++ updateLetterBoard xs ys guess
updateLetterBoard _ _ _ = []
