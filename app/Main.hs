module Main where

import Hangman

promptForGamePhrase :: IO String
promptForGamePhrase = do
  putStrLn "Which should the game phrase be?"
  putStrLn "Enter any word:"
  line <- getLine
  pure line

playGame :: (String, String, String, Int) -> IO ()
playGame (gamePhrase, currentLetterBoard, guessList, guesses) = do
--   printGallows guesses -- This can be ASCII output
  putStrLn currentLetterBoard
  putStrLn ("Guesses so far: " ++ (show guesses))
  if checkIfWon gamePhrase currentLetterBoard then
    putStrLn "You win!"
  else if guesses > 10 then
    putStrLn "You lose."
  else do
    g <- promptGuess
    -- updateLetterBoard
    -- updateGuesses -- just a inline increment?
    -- updateGuessList
    playGame(gamePhrase, (updateLetterBoard gamePhrase currentLetterBoard g), [], guesses+1)

main :: IO ()
main = do
    phrase <- promptForGamePhrase
    playGame (phrase, (setupLetterBoard phrase), [], 0)
