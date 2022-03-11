module Main where

import Escape

promptForGamePhrase :: IO String
promptForGamePhrase = do
  putStrLn "Which should the game winning escape phrase be?"
  putStrLn "Enter any phrase:"
  line <- getLine
  pure line

playGame :: (String, LetterBoard, Guesses) -> IO ()
playGame (gamePhrase, currentLetterBoard, (guessList, guessCount)) = do
  printMonster guessCount
  putStrLn " "
  putStrLn ("Escape phrase: " ++ currentLetterBoard)
  putStrLn ("Missed guesses so far: " ++ guessList)
  if checkIfWon gamePhrase currentLetterBoard then
    putStrLn ("You cry out the magical phrase '" ++ gamePhrase ++ "' and escape!")
  else if guessCount > 5 then
    putStrLn "You died."
  else do
    g <- promptGuess
    playGame(gamePhrase, (updateLetterBoard gamePhrase currentLetterBoard g), (updateGuesses gamePhrase currentLetterBoard (guessList, guessCount) g))

main :: IO ()
main = do
    phrase <- promptForGamePhrase
    playGame (phrase, (setupLetterBoard phrase), ([], 0))
