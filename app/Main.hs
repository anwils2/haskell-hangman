module Main where

import Hangman

promptForGamePhrase :: IO String
promptForGamePhrase = do
  putStrLn "Which should the game phrase be?"
  putStrLn "Enter any word:"
  line <- getLine
  pure line

playGame :: (String, LetterBoard, Guesses) -> IO ()
playGame (gamePhrase, currentLetterBoard, (guessList, guessCount)) = do
--   printGallows guesses -- This can be ASCII output
  putStrLn currentLetterBoard
  putStrLn ("Missed guesses so far: " ++ guessList)
  if checkIfWon gamePhrase currentLetterBoard then
    putStrLn "You win!"
  else if guessCount > 5 then
    putStrLn "You lose."
  else do
    g <- promptGuess
    playGame(gamePhrase, (updateLetterBoard gamePhrase currentLetterBoard g), (updateGuesses gamePhrase currentLetterBoard (guessList, guessCount) g))

main :: IO ()
main = do
    phrase <- promptForGamePhrase
    fakeClearTerminal
    playGame (phrase, (setupLetterBoard phrase), ([], 0))
