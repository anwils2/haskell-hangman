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
  printMonster guessCount
  putStrLn " "
  putStrLn ("Escape word: " ++ currentLetterBoard)
  putStrLn ("Missed guesses so far: " ++ guessList)
  if checkIfWon gamePhrase currentLetterBoard then
    putStrLn "You escaped!"
  else if guessCount > 5 then
    putStrLn "You died."
  else do
    g <- promptGuess
    playGame(gamePhrase, (updateLetterBoard gamePhrase currentLetterBoard g), (updateGuesses gamePhrase currentLetterBoard (guessList, guessCount) g))

main :: IO ()
main = do
    phrase <- promptForGamePhrase
    playGame (phrase, (setupLetterBoard phrase), ([], 0))
