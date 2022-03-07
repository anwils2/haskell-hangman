module Hangman where

import Data.List
import Data.Char

type LetterBoard = String

type Guesses = (String, Int)

setupLetterBoard :: String -> LetterBoard
setupLetterBoard [] = []
setupLetterBoard (x : xs) = "*" ++ setupLetterBoard xs

checkOneChar :: String -> Bool
checkOneChar (x : xs) =
    if xs == [] then
        True
    else
        False
checkOneChar [] = False

promptGuess :: IO String
promptGuess = do
  putStrLn "Guess a letter:"
  line <- getLine
  if (checkOneChar line) then do
    pure line
  else do
      putStrLn "Guess must be a single letter, try again"
      promptGuess

checkIfWon :: String -> LetterBoard -> Bool
checkIfWon x y = 
    if x == y then
        True
    else
        False

checkGuessInList :: String -> String -> Bool
checkGuessInList (x : xs) (guess:guessTail) = 
    if x == guess then
        True
    else
        checkGuessInList xs (guess:guessTail)
checkGuessInList _ _ = False

updateGuesses :: String -> LetterBoard-> Guesses -> String -> Guesses
updateGuesses gamePhrase currentLetterBoard (guessList, i) guess =
    if checkGuessInList (currentLetterBoard ++ guessList) guess then -- Already guess
        (guessList, i)
    else if checkGuessInList gamePhrase guess then -- correct guess
        (guessList, i)
    else -- incorrect guess
        (guessList ++ guess, i+1)


updateLetterBoard :: String -> LetterBoard -> String -> LetterBoard
updateLetterBoard (x : xs) (y : ys) (guess : guesses) =
    if x == y then -- letter has been guessed
        [y] ++ updateLetterBoard xs ys (guess : guesses)
    else if x == guess then -- letter hasn't been guessed, but matches gamePhrase letter
        [guess] ++ updateLetterBoard xs ys (guess : guesses)
    else
        "*" ++ updateLetterBoard xs ys (guess : guesses)
updateLetterBoard _ _ _ = []

    -- putStrLn "         _.---._                                 "
    -- putStrLn "        /       \\                               "
    -- putStrLn "      | .- ,-   |____                            "
    -- putStrLn "       \  *) *) (\/__/\                          "
    -- putStrLn "        \   ^` (/\\  \-\                        "
    -- putStrLn "         `v"uuV` )/   )-)                       "
    -- putStrLn "          |   ^nn^   /-/   _______.-.______.-,._"
    -- putStrLn "          |    __,-.(-(.-'(_.-----`-'------.\ \ `"
    -- putStrLn "          |   ( (   /\  /  )               \) |"
    -- putStrLn "          .   //(`._'`_' / )              (_|_|_"
    -- putStrLn "             // (`._/\`-' /               `-' '-)"
    -- putStrLn "            //   `._/ `.-'                  / |"
    -- putStrLn "           /(      .-.\-\.-.                | |"
    -- putStrLn "          (v)     (  ( \-)  )               ) |"
    -- putStrLn "          //       \  `-'  /_              /  |"
    -- putStrLn "         //      ( '`--^--'v )             )  |"
    -- putStrLn "        //        \(      ) /  _  , , ,    \  /"
    -- putStrLn "  \ - -'_`.        \\    //  _/ \`_/       |  |"
    -- putStrLn "    `     `) | o__  \\  //,-'_)-'           ) |"
    -- putStrLn "        /  \ T/|)_)  )\/(.-.____.-/ \._     \ |"
    -- putStrLn "              / \   (_(._)_.----._)_)._)_>   \/"

printMonster :: Int -> IO ()
printMonster i = do
    if i == 0 then do
        putStrLn "         "
        putStrLn "        "
        putStrLn "       "
        putStrLn "       "
        putStrLn "        "
        putStrLn "         "
        putStrLn "          "
        putStrLn "          "
        putStrLn "          "
        putStrLn "          "
        putStrLn "             "
        putStrLn "            "
        putStrLn "           "
        putStrLn "          "
        putStrLn "          "
        putStrLn "         "
        putStrLn "        "
        putStrLn " \\ - -'_`.        "
        putStrLn "    `     `) | o__  "
        putStrLn "        /  \\ T/|)_)"
        putStrLn "              / \\   "
    else if i == 1 then do
        putStrLn "         "
        putStrLn "        "
        putStrLn "       "
        putStrLn "       "
        putStrLn "        "
        putStrLn "         "
        putStrLn "          "
        putStrLn "               __"
        putStrLn "              ( ("
        putStrLn "              //"
        putStrLn "             // "
        putStrLn "            //   "
        putStrLn "           /(      "
        putStrLn "          (v)     "
        putStrLn "          //       "
        putStrLn "         //      "
        putStrLn "        //        "
        putStrLn " \\ - -'_`.        "
        putStrLn "    `     `) | o__  "
        putStrLn "        /  \\ T/|)_)  "
        putStrLn "              / \\   "
    else if i == 2 then do
        putStrLn "         "
        putStrLn "        "
        putStrLn "       "
        putStrLn "       "
        putStrLn "        "
        putStrLn "         "
        putStrLn "          "
        putStrLn "               __,-.(-(.-\'("
        putStrLn "              ( (   /\\  /  )        "
        putStrLn "              //(`._'`_' / )           "
        putStrLn "             // (`._/\\`-' /          "
        putStrLn "            //   `._/ `.-'                 "
        putStrLn "           /(      .-.\\-\\.-. "
        putStrLn "          (v)     (  ( \\-)  ) "
        putStrLn "          //       \\  `-'  /"
        putStrLn "         //      "
        putStrLn "        //        "
        putStrLn " \\ - -'_`.        "
        putStrLn "    `     `) | o__  "
        putStrLn "        /  \\ T/|)_)  "
        putStrLn "              / \\   "
    else if i == 3 then do
        putStrLn "         "
        putStrLn "        "
        putStrLn "       "
        putStrLn "       "
        putStrLn "        "
        putStrLn "         "
        putStrLn "          "
        putStrLn "               __,-.(-(.-\'(`"
        putStrLn "              ( (   /\\  /  )               "
        putStrLn "              //(`._'`_' / )             "
        putStrLn "             // (`._/\\`-' /         "
        putStrLn "            //   `._/ `.-'                  "
        putStrLn "           /(      .-.\\-\\.-.              "
        putStrLn "          (v)     (  ( \\-)  )              "
        putStrLn "          //       \\  `-'  /_              "
        putStrLn "         //        '`--^--'v ) "
        putStrLn "        //                ) / "
        putStrLn " \\ - -'_`.               // "
        putStrLn "    `     `) | o__      //"
        putStrLn "        /  \\ T/|)_)    /(.-.____.-/ \\._"
        putStrLn "              / \\     (._)_.----._)_)._)_>"
    else if i == 4 then do
        putStrLn "         "
        putStrLn "        "
        putStrLn "       "
        putStrLn "       "
        putStrLn "        "
        putStrLn "         "
        putStrLn "          "
        putStrLn "               __,-.(-(.-\'(`"
        putStrLn "              ( (   /\\  /  )              "
        putStrLn "              //(`._'`_' / )              "
        putStrLn "             // (`._/\\`-' /              "
        putStrLn "            //   `._/ `.-'                  "
        putStrLn "           /(      .-.\\-\\.-.              "
        putStrLn "          (v)     (  ( \\-)  )               "
        putStrLn "          //       \\  `-'  /_              "
        putStrLn "         //      ( '`--^--'v )             "
        putStrLn "        //        \\(      ) /  _  , , ,  "
        putStrLn " \\ - -'_`.         \\\\    //  _/ \\`_/    "
        putStrLn "    `     `) | o__  \\\\  //,-'_)-'         "
        putStrLn "        /  \\ T/|)_)  )\\/(.-.____.-/ \\._ "
        putStrLn "              / \\   (_(._)_.----._)_)._)_>"
    else if i == 5 then do
        putStrLn "         "
        putStrLn "        "
        putStrLn "       "
        putStrLn "       "
        putStrLn "        "
        putStrLn "         "
        putStrLn "                           _______.-.______.-,._"
        putStrLn "               __,-.(-(.-\'(_.-----`-'------.\\ \\ `"
        putStrLn "              ( (   /\\  /  )               \\) |"
        putStrLn "              //(`._'`_' / )              (_|_|_"
        putStrLn "             // (`._/\\`-' /               `-' '-)"
        putStrLn "            //   `._/ `.-'                  / |"
        putStrLn "           /(      .-.\\-\\.-.                | |"
        putStrLn "          (v)     (  ( \\-)  )               ) |"
        putStrLn "          //       \\  `-'  /_              /  |"
        putStrLn "         //      ( '`--^--'v )             )  |"
        putStrLn "        //        \\(      ) /  _  , , ,    \\  /"
        putStrLn " \\ - -'_`.         \\\\    //  _/ \\`_/       |  |"
        putStrLn "    `     `) | o__  \\\\  //,-'_)-'           ) |"
        putStrLn "        /  \\ T/|)_)  )\\/(.-.____.-/ \\._     \\ |"
        putStrLn "              / \\   (_(._)_.----._)_)._)_>   \\/"
    else if i == 6 then do
        putStrLn "         _.---._                                 "
        putStrLn "        /       \\                               "
        putStrLn "       | .- ,-   |____                            "
        putStrLn "       \\  *) *) (\\/__/\\                          "
        putStrLn "        \\   ^` (/\\\\  \\-\\                        "
        putStrLn "         `v\"uuV` )/   )-)                       "
        putStrLn "          |   ^nn^   /-/   _______.-.______.-,._"
        putStrLn "          |    __,-.(-(.-\'(_.-----`-'------.\\ \\ `"
        putStrLn "          |   ( (   /\\  /  )               \\) |"
        putStrLn "          .   //(`._'`_' / )              (_|_|_"
        putStrLn "             // (`._/\\`-' /               `-' '-)"
        putStrLn "            //   `._/ `.-'                  / |"
        putStrLn "           /(      .-.\\-\\.-.                | |"
        putStrLn "          (v)     (  ( \\-)  )               ) |"
        putStrLn "          //       \\  `-'  /_              /  |"
        putStrLn "         //      ( '`--^--'v )             )  |"
        putStrLn "        //        \\(      ) /  _  , , ,    \\  /"
        putStrLn " \\ - -'_`.         \\\\    //  _/ \\`_/       |  |"
        putStrLn "    `     `) | o__  \\\\  //,-'_)-'           ) |"
        putStrLn "        /  \\ T/|)_)  )\\/(.-.____.-/ \\._     \\ |"
        putStrLn "              / \\   (_(._)_.----._)_)._)_>   \\/"
    else
        putStrLn ""
